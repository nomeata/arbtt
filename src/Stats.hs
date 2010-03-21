{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Stats where

import Data.Time
import Data.Maybe
import Data.List
import Data.Ord
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Data.MyText (Text)

import Data
import Categorize


data Report = GeneralInfos | TotalTime | Category Text | EachCategory
        deriving (Show, Eq)

data Filter = Exclude Activity | Only Activity | AlsoInactive | GeneralCond String
        deriving (Show, Eq)

-- Supported report output formats: text, comma-separated values and
-- tab-separated values
data ReportFormat = Text | CSV | TSV
        deriving (Show, Eq, Read)

data ReportOption = MinPercentage Double | OutputFormat ReportFormat
        deriving (Show, Eq)

-- Data format semantically representing the result of a report, including the
-- title
data ReportResults =
        ListOfFields String [(String, String)]
        | ListOfTimePercValues String [(String, String, Double)]
        | PieChartOfTimePercValues  String [(String, String, Double)]


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
        , totalTimeRec :: NominalDiffTime
        , totalTimeSel :: NominalDiffTime
        , fractionRec :: Double
        , fractionSel :: Double
        , fractionSelRec :: Double
        , sums :: M.Map Activity NominalDiffTime
        , allTags :: TimeLog (Ctx, ActivityData)
        , tags :: TimeLog (Ctx, ActivityData)
        }

prepareCalculations :: TimeLog (Ctx, ActivityData) -> TimeLog (Ctx, ActivityData) -> Calculations
prepareCalculations allTags tags =
  let c = Calculations
          { firstDate = tlTime (head allTags)
          , lastDate = tlTime (last allTags)
          , timeDiff = diffUTCTime (lastDate c) (firstDate c)
          , totalTimeRec = fromInteger (sum (map tlRate allTags))/1000
          , totalTimeSel = fromInteger (sum (map tlRate tags))/1000
          , fractionRec = realToFrac (totalTimeRec c) / (realToFrac (timeDiff c))
          , fractionSel = realToFrac (totalTimeSel c) / (realToFrac (timeDiff c))
          , fractionSelRec = realToFrac (totalTimeSel c) / realToFrac (totalTimeRec c)
          , sums = sumUp tags
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

putReports :: [ReportOption] -> Calculations -> [Report] -> IO ()
putReports opts c = sequence_ . intersperse (putStrLn "") . map (putReport opts c) 

putReport :: [ReportOption] -> Calculations -> Report -> IO ()
putReport opts c EachCategory = putReports opts c (map Category (listCategories (tags c)))
putReport opts c r = renderReport opts $ reportToTable opts c r

reportToTable :: [ReportOption] -> Calculations -> Report -> ReportResults
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
                      if perc*100 >= minPercentage
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
                    tooSmallSums = M.filter (\t -> realToFrac t / realToFrac totalTimeSel * 100 < minPercentage) filteredSums
                    tooSmallTimes = M.fold (+) 0 tooSmallSums
                in

                mapMaybe (\(tag,time) ->
                      let perc = realToFrac time/realToFrac totalTimeSel in
                      if perc*100 >= minPercentage
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
    where
        minPercentage = 
            case pvalues of
                [] -> 1
                _  -> last pvalues
        pvalues = mapMaybe pickPercentage opts
        pickPercentage o = case o of
            MinPercentage m -> Just m
            _ -> Nothing

renderReport opts reportdata = do
    let results = doRender opts reportdata
    putStr results

doRender opts reportdata = results
    where
        results =
            case outputformat of
                Text -> renderReportText reportdata
                CSV -> renderReportCSV reportdata
                TSV -> renderReportTSV reportdata
        outputformat =
            case formats of
                [] -> Text
                _  -> last formats
        formats = mapMaybe pickFormats opts
        pickFormats o = case o of
            OutputFormat f -> Just f
            _ -> Nothing

renderReportText (ListOfFields title dats) = 
    underline title ++
    (tabulate False $ map (\(f,v) -> [f,v]) dats)

renderReportText (ListOfTimePercValues title dats) = 
    underline title ++ (tabulate True $ listOfValues dats)

renderReportText (PieChartOfTimePercValues title dats) = 
    underline title ++ (tabulate True $ piechartOfValues dats)

listOfValues dats =
    ["Tag","Time","Percentage"] :
    map (\(f,t,p) -> [f,t,printf "%.2f" (p*100)]) dats

piechartOfValues dats =
    ["Tag","Time","Percentage"] :
    map (\(f,t,p) -> [f,t,printf "%.2f" (p*100)]) dats

-- The reporting of "General Information" is not supported for the
-- comma-separated output format.
renderReportCSV (ListOfFields title dats) = 
    error ("\"" ++ title ++ "\"" ++ " not supported for comma-separated output format")

renderReportCSV (ListOfTimePercValues title dats) = 
    unlines $ map (injectDelimiter ",") (listOfValues dats)

renderReportCSV (PieChartOfTimePercValues _ dats) = 
    unlines $ map (injectDelimiter ",") (piechartOfValues dats)

-- The reporting of "General Information" is not supported for the
-- TAB-separated output format.
renderReportTSV (ListOfFields title dats) = 
    error ("\"" ++ title ++ "\"" ++ " not supported for TAB-separated output format")

renderReportTSV (ListOfTimePercValues title dats) = 
    unlines $ map (injectDelimiter "\t") (listOfValues dats)

renderReportTSV (PieChartOfTimePercValues _ dats) = 
    unlines $ map (injectDelimiter "\t") (piechartOfValues dats)

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

underline str = unlines 
    [ str
    , map (const '=') str
    ]
