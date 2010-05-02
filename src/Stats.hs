{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Stats where

import Data.Time
import Data.Maybe
import Data.List
import Data.Ord
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Data.MyText (Text,unpack)
import Data.Function (on)
import System.Locale (defaultTimeLocale)


import Data
import Categorize


data Report = GeneralInfos
    | TotalTime
    | Category Category
    | EachCategory
    | IntervalCategory Category
    | IntervalTag Activity
        deriving (Show, Eq)

data Filter = Exclude Activity | Only Activity | GeneralCond String
        deriving (Show, Eq)

-- Supported report output formats: text, comma-separated values and
-- tab-separated values
data ReportFormat = RFText | RFCSV | RFTSV
        deriving (Show, Eq)

data ReportOptions = ReportOptions
    { roMinPercentage :: Double
    , roReportFormat :: ReportFormat
    }
        deriving (Show, Eq)

defaultReportOptions = ReportOptions
    { roMinPercentage = 1
    , roReportFormat = RFText
    }

-- Data format semantically representing the result of a report, including the
-- title
data ReportResults =
        ListOfFields String [(String, String)]
        | ListOfTimePercValues String [(String, String, Double)]
        | PieChartOfTimePercValues  String [(String, String, Double)]
        | ListOfIntervals String [(String,String,String,String)]


-- We apply the filters in a way such that consecutive runs of selected samples
-- are in the same sublist, and sublists are separated by non-selected samples
applyFilters :: [Filter] -> TimeLog (Ctx, ActivityData) -> [TimeLog (Ctx, ActivityData)]
applyFilters filters = filterAndSeparate $ \tl ->
       all (\flag -> case flag of 
                Exclude act  -> excludeTag act tl
                Only act     -> onlyTag act tl
                GeneralCond s-> applyCond s tl) filters

filterAndSeparate :: (a -> Bool) -> [a] -> [[a]]
filterAndSeparate pred = fst . go
  where go [] = ([],True)
        go (x:xs) = case (go xs,pred x) of
                    ((rs,     True) , True)  -> ([x]:rs,   False)
                    (((r:rs), False), True)  -> ((x:r):rs, False)
                    ((rs,     _)    , False) -> (rs,       True)
                                
excludeTag act = notElem act . snd . tlData
onlyTag act = elem act . snd . tlData
defaultFilter = Exclude inactiveActivity

-- | to be used lazily, to re-use computation when generating more than one
-- report at a time
data Calculations = Calculations
        { firstDate :: UTCTime
        , lastDate  :: UTCTime
        , timeDiff :: NominalDiffTime
        , totalTimeRec :: NominalDiffTime
        , totalTimeSel :: NominalDiffTime
        , fractionRec :: Double
        , fractionSel :: Double
        , fractionSelRec :: Double
        , sums :: M.Map Activity NominalDiffTime
        , allTags :: TimeLog (Ctx, ActivityData)
        -- tags is a list of uninterrupted entries
        , tags :: [TimeLog (Ctx, ActivityData)]
        }

prepareCalculations :: TimeLog (Ctx, ActivityData) -> [TimeLog (Ctx, ActivityData)] -> Calculations
prepareCalculations allTags tags =
  let c = Calculations
          { firstDate = tlTime (head allTags)
          , lastDate = tlTime (last allTags)
          , timeDiff = diffUTCTime (lastDate c) (firstDate c)
          , totalTimeRec = fromInteger (sum (map tlRate allTags))/1000
          , totalTimeSel = fromInteger (sum (map tlRate (concat tags)))/1000
          , fractionRec = realToFrac (totalTimeRec c) / (realToFrac (timeDiff c))
          , fractionSel = realToFrac (totalTimeSel c) / (realToFrac (timeDiff c))
          , fractionSelRec = realToFrac (totalTimeSel c) / realToFrac (totalTimeRec c)
          , sums = sumUp (concat tags)
          , allTags
          , tags
          } in c

-- | Sums up each occurence of an 'Activity', weighted by the sampling rate
sumUp :: TimeLog (Ctx, ActivityData) -> M.Map Activity NominalDiffTime
sumUp = foldr go M.empty
  where go tl m = foldr go' m (snd (tlData tl))
          where go' act = M.insertWith (+) act (fromInteger (tlRate tl)/1000)


listCategories :: TimeLog (Ctx, ActivityData) -> [Category]
listCategories = S.toList . foldr go S.empty
  where go tl m = foldr go' m (snd (tlData tl))
          where go' (Activity (Just cat) _) = S.insert cat
                go' _                       = id

putReports :: ReportOptions -> Calculations -> [Report] -> IO ()
putReports opts c = sequence_ . intersperse (putStrLn "") . map (putReport opts c) 

putReport :: ReportOptions -> Calculations -> Report -> IO ()
putReport opts c EachCategory = putReports opts c (map Category (listCategories (concat (tags c))))
putReport opts c r = renderReport opts $ reportToTable opts c r

reportToTable :: ReportOptions -> Calculations -> Report -> ReportResults
reportToTable opts (Calculations {..}) r = case r of
        GeneralInfos -> ListOfFields "General Information" $
                [ ("FirstRecord", show firstDate)
                , ("LastRecord",  show lastDate)
                , ("Number of records", show (length allTags))
                , ("Total time recorded",  showTimeDiff totalTimeRec)
                , ("Total time selected",  showTimeDiff totalTimeSel)
                , ("Fraction of total time recorded", printf "%3.0f%%" (fractionRec * 100))
                , ("Fraction of total time selected", printf "%3.0f%%" (fractionSel * 100))
                , ("Fraction of recorded time selected", printf "%3.0f%%" (fractionSelRec * 100))
                ]

        TotalTime -> ListOfTimePercValues "Total time per tag" $
                mapMaybe (\(tag,time) ->
                      let perc = realToFrac time/realToFrac totalTimeSel in
                      if perc*100 >= roMinPercentage opts
                      then Just $ ( show tag
                                  , showTimeDiff time
                                  , perc)
                      else Nothing
                      ) $
                reverse $
                sortBy (comparing snd) $
                M.toList sums
        
        Category cat -> PieChartOfTimePercValues ("Statistics for category " ++ show cat) $
                let filteredSums = M.filterWithKey (\a _ -> isCategory cat a) sums
                    uncategorizedTime = totalTimeSel - M.fold (+) 0 filteredSums
                    tooSmallSums = M.filter (\t -> realToFrac t / realToFrac totalTimeSel * 100 < roMinPercentage opts) filteredSums
                    tooSmallTimes = M.fold (+) 0 tooSmallSums
                in

                mapMaybe (\(tag,time) ->
                      let perc = realToFrac time/realToFrac totalTimeSel in
                      if perc*100 >= roMinPercentage opts
                      then Just ( show tag
                                , showTimeDiff time
                                , perc)
                      else Nothing
                      )
                      (reverse $ sortBy (comparing snd) $ M.toList filteredSums)
                ++
                (
                if tooSmallTimes > 0
                then [( printf "(%d entries omitted)" (M.size tooSmallSums)
                      , showTimeDiff tooSmallTimes
                      , realToFrac tooSmallTimes/realToFrac totalTimeSel
                      )]
                else []
                )
                ++      
                (if uncategorizedTime > 0
                then [( "(unmatched time)"
                      , showTimeDiff uncategorizedTime
                      , realToFrac uncategorizedTime/realToFrac totalTimeSel
                      )]
                else []
                )
        
        IntervalCategory cat -> intervalReportToTable ("Intervals for category " ++ show cat)
                                                      (extractCat cat) 
        IntervalTag tag -> intervalReportToTable ("Intervals for category " ++ show tag)
                                                 (extractTag tag) 

    where
        extractCat :: Category -> ActivityData -> Maybe String
        extractCat cat = fmap (unpack . activityName) . listToMaybe . filter ( (==Just cat) . activityCategory )

        extractTag :: Activity -> ActivityData -> Maybe String
        extractTag tag = fmap show . listToMaybe . filter ( (==tag) )

        intervalReportToTable :: String -> (ActivityData -> Maybe String) -> ReportResults
        intervalReportToTable title extr = ListOfIntervals title $
            map (\tles ->
                let str = fromJust (tlData (head tles))
                    firstE = showUtcTime (tlTime (head tles))
                    lastE = showUtcTime (tlTime (last tles))
                    timeLength = showTimeDiff $
                        tlTime (last tles) `diffUTCTime` tlTime (head tles) +
                        fromIntegral (tlRate (last tles))/1000
                in (str, firstE, lastE, timeLength)) $
            filter (isJust . tlData . head ) $
            concat $
            fmap (groupBy ((==) `on` tlData) .
                 (fmap.fmap) (extr . snd)) $
            tags
            
            

renderReport opts reportdata = do
    let results = doRender opts reportdata
    putStr results

doRender opts reportdata = case roReportFormat opts of
                RFText -> renderReportText reportdata
                RFCSV -> renderReportCSV reportdata
                RFTSV -> renderReportTSV reportdata

renderReportText (ListOfFields title dats) = 
    underline title ++
    (tabulate False $ map (\(f,v) -> [f,v]) dats)

renderReportText (ListOfTimePercValues title dats) = 
    underline title ++ (tabulate True $ listOfValues dats)

renderReportText (PieChartOfTimePercValues title dats) = 
    underline title ++ (tabulate True $ piechartOfValues dats)

renderReportText (ListOfIntervals title dats) = 
    underline title ++ (tabulate True $ listOfIntervals dats)

listOfValues dats =
    ["Tag","Time","Percentage"] :
    map (\(f,t,p) -> [f,t,printf "%.2f" (p*100)]) dats

piechartOfValues dats =
    ["Tag","Time","Percentage"] :
    map (\(f,t,p) -> [f,t,printf "%.2f" (p*100)]) dats

listOfIntervals dats =
    ["Tag","From","Until","Duration"] :
    map (\(t,f,u,d) -> [t,f,u,d]) dats

-- The reporting of "General Information" is not supported for the
-- comma-separated output format.
renderReportCSV (ListOfFields title dats) = 
    error ("\"" ++ title ++ "\"" ++ " not supported for comma-separated output format")

renderReportCSV (ListOfTimePercValues _ dats) = 
    renderWithDelimiter "," (listOfValues dats)

renderReportCSV (PieChartOfTimePercValues _ dats) = 
    renderWithDelimiter "," (piechartOfValues dats)

renderReportCSV (ListOfIntervals title dats) = 
    renderWithDelimiter "," (listOfIntervals dats)

-- The reporting of "General Information" is not supported for the
-- TAB-separated output format.
renderReportTSV (ListOfFields title dats) = 
    error ("\"" ++ title ++ "\"" ++ " not supported for TAB-separated output format")

renderReportTSV (ListOfTimePercValues _ dats) = 
    renderWithDelimiter "\t" (listOfValues dats)

renderReportTSV (PieChartOfTimePercValues _ dats) = 
    renderWithDelimiter "\t" (piechartOfValues dats)

renderReportTSV (ListOfIntervals title dats) = 
    renderWithDelimiter "\t" (listOfIntervals dats)

renderWithDelimiter delim datasource =
    unlines $ map (injectDelimiter delim) datasource

injectDelimiter d = concat . intersperse d

tabulate :: Bool -> [[String]] -> String
tabulate titlerow rows = unlines $ addTitleRow $ map (intercalate " | " . zipWith (\l s -> take (l - length s) (repeat ' ') ++ s) colwidths) rows
  where cols = transpose rows
        colwidths = map (maximum . map length) cols
        addTitleRow | titlerow  = \(l:ls) -> (map (\c -> if c == ' ' then '_' else c) l ++ "_")
                                             : ls
                 -- | titlerow  = \(l:ls) -> l : (take (length l) (repeat '-')) : ls
                    | otherwise = id

showTimeDiff :: NominalDiffTime -> String
showTimeDiff t = go False $ zip [days,hours,mins,secs] ["d","h","m","s"]
  where s = round t :: Integer
        days  =  s `div` (24*60*60)
        hours = (s `div` (60*60)) `mod` 24
        mins  = (s `div` 60) `mod` 60
        secs  =  s `mod` 60 
        go False []         = "0s"
        go True  []         = ""
--      go True  vs         | all (==0) (map fst vs) = concat (replicate (length vs) "   ")
        go True  ((a,u):vs)             = printf "%02d%s" a u ++ go True vs
        go False ((a,u):vs) | a > 0     = printf "%2d%s" a u ++ go True vs
                            | otherwise =                       go False vs

showUtcTime :: UTCTime -> String
showUtcTime = formatTime defaultTimeLocale "%x %X"

underline str = unlines 
    [ str
    , map (const '=') str
    ]
