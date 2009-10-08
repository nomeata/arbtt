{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Stats where

import Data.Time
import Data.Maybe
import Data.List
import Data.Ord
import Text.Tabular
import qualified Text.Tabular.AsciiArt as TA
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S

import Data
import Categorize


data Report = GeneralInfos | TotalTime | Category String | EachCategory
        deriving Eq

data Filter = Exclude Activity | Only Activity | AlsoInactive | GeneralCond String
        deriving Eq

data ReportOption = MinPercentage Double
        deriving Eq

applyFilters :: [Filter] -> TimeLog (Ctx, ActivityData) -> TimeLog (Ctx, ActivityData)
applyFilters filters tle = 
        foldr (\flag -> case flag of 
                                Exclude act  -> excludeTag act
                                Only act     -> onlyTag act
                                AlsoInactive -> id
				GeneralCond s-> applyCond s 
        ) (if AlsoInactive `elem` filters then tle else defaultFilter tle) filters


excludeTag act = filter (notElem act . snd . tlData)
onlyTag act = filter (elem act . snd . tlData)
defaultFilter = excludeTag inactiveActivity

-- | to be used lazily, to re-use computation when generating more than one
-- report at a time
data Calculations = Calculations
	{ firstDate :: UTCTime
	, lastDate  :: UTCTime
	, timeDiff :: NominalDiffTime
	, totalTimeRec :: Integer
	, totalTimeSel :: Integer
	, fractionRec :: Double
	, fractionSel :: Double
	, fractionSelRec :: Double
	, sums :: M.Map Activity Integer
	, allTags :: TimeLog (Ctx, ActivityData)
	, tags :: TimeLog (Ctx, ActivityData)
	}

prepareCalculations :: TimeLog (Ctx, ActivityData) -> TimeLog (Ctx, ActivityData) -> Calculations
prepareCalculations allTags tags =
  let c = Calculations
	  { firstDate = tlTime (head allTags)
	  , lastDate = tlTime (last allTags)
	  , timeDiff = diffUTCTime (lastDate c) (firstDate c)
	  , totalTimeRec = sum (map tlRate allTags)
	  , totalTimeSel = sum (map tlRate tags)
	  , fractionRec = fromIntegral (totalTimeRec c) / (realToFrac (timeDiff c) * 1000)
	  , fractionSel = fromIntegral (totalTimeSel c) / (realToFrac (timeDiff c) * 1000)
	  , fractionSelRec = fromIntegral (totalTimeSel c) / fromIntegral (totalTimeRec c)
	  , sums = sumUp tags
	  , allTags
	  , tags
	  } in c

-- | Sums up each occurence of an 'Activity', weighted by the sampling rate
sumUp :: TimeLog (Ctx, ActivityData) -> M.Map Activity Integer
sumUp = foldr go M.empty
  where go tl m = foldr go' m (snd (tlData tl))
          where go' act = M.insertWith (+) act (tlRate tl)


listCategories :: TimeLog (Ctx, ActivityData) -> [Category]
listCategories = S.toList . foldr go S.empty
  where go tl m = foldr go' m (snd (tlData tl))
          where go' (Activity (Just cat) _) = S.insert cat
	        go' _                       = id

putReports :: [ReportOption] -> Calculations -> [Report] -> IO ()
putReports opts c = sequence_ . intersperse (putStrLn "") . map (putReport opts c) 

putReport :: [ReportOption] -> Calculations -> Report -> IO ()
putReport opts c EachCategory = putReports opts c (map Category (listCategories (tags c)))
putReport opts c r = let (h,t) = reportToTable opts c r
  			in putStrLnUnderlined h >> putStr (TA.render id id id t)

reportToTable :: [ReportOption] -> Calculations -> Report -> (String, Table String String String)
reportToTable opts (Calculations {..}) r = case r of
 	GeneralInfos -> ("General Information",
		empty ^..^ colH "Value"
		+.+ row "FirstRecord"
		        [show firstDate]
		+.+ row "LastRecord"
		        [show lastDate]
		+.+ row "Number of records"
		        [show (length allTags)]
		+.+ row "Total time recorded"
		        [formatSeconds (fromIntegral totalTimeRec / 1000)]
		+.+ row "Total time selected"
		        [formatSeconds (fromIntegral totalTimeSel / 1000)]
		+.+ row "Fraction of total time recorded"
		        [printf "%3.0f%%" (fractionRec * 100) ]
		+.+ row "Fraction of total time selected"
		        [printf "%3.0f%%" (fractionSel * 100) ]
		+.+ row "Fraction of recorded time selected"
		        [printf "%3.0f%%" (fractionSelRec * 100) ]
		)

 	TotalTime -> ("Total time per tag",
		foldr (\(tag,time) ->
		      let perc = fromIntegral time/fromIntegral totalTimeSel*100 in
		      if perc >= minPercentage
	 	      then (+.+ row (show tag) [
		      		formatSeconds (fromIntegral time/1000),
				printf "%.1f%%" perc])
		      else id
		      )
		(empty ^..^ colH "Time" ^..^ colH "Percentage")
		(sortBy (comparing snd) $ M.toList sums)
		)
	
	Category cat -> ("Statistics for category " ++ cat,
         	let filteredSums = M.filterWithKey (\a _ -> isCategory cat a) sums
	            uncategorizedTime = totalTimeSel - M.fold (+) 0 filteredSums
          	    tooSmallSums = M.filter (\t -> fromIntegral t / fromIntegral totalTimeSel * 100 < minPercentage) filteredSums
	  	    tooSmallTimes = M.fold (+) 0 tooSmallSums
		in

		(if uncategorizedTime > 0
		then (+.+ row "(unmatched time)" [
                        formatSeconds (fromIntegral uncategorizedTime/1000),
                        printf "%.1f%%" (fromIntegral uncategorizedTime/fromIntegral totalTimeSel*100::Double)])
		else id
		)
		.	
		(
		if tooSmallTimes > 0
		then (+.+ row (printf "(%d entries omitted)" (M.size tooSmallSums)) [
                        formatSeconds (fromIntegral tooSmallTimes/1000),
                        printf "%.1f%%" (fromIntegral tooSmallTimes/fromIntegral totalTimeSel*100::Double) ])
		else id
		)
		$	
		foldr (\(tag,time) ->
		      let perc = fromIntegral time/fromIntegral totalTimeSel*100 in
		      if perc >= minPercentage
	 	      then (+.+ row (show tag) [
		      		formatSeconds (fromIntegral time/1000),
				printf "%.1f%%" perc])
		      else id
		      )

		(empty ^..^ colH "Time" ^..^ colH "Percentage")

          	(sortBy (comparing snd) $ M.toList filteredSums)
		)

  where minPercentage = last $ mapMaybe (\f -> case f of {MinPercentage m -> Just m {- ; _ -> Nothing -} }) opts


formatSeconds :: Double -> String
formatSeconds s' = go $ zip [days,hours,mins,secs] ["d","h","m","s"]
  where s = round s' :: Integer
        days  =  s `div` (24*60*60)
        hours = (s `div` (60*60)) `mod` 24
        mins  = (s `div` 60) `mod` 60
	secs  =  s `mod` 60 
	go | s == 0    = const "0s"
	   | otherwise = concat . snd . mapAccumL go' False 
	go' True  (a,u)             = (True, printf "%02d%s" a u)
	go' False (a,u) | a > 0     = (True, printf "%2d%s" a u)
	                | otherwise = (False, "")

putStrLnUnderlined str = do
        putStrLn str
        putStrLn $ map (const '=') str
