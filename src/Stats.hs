{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeOperators, TupleSections, GADTSyntax, ExistentialQuantification #-}
module Stats (
    Report(..),
    ReportOptions(..),
    ReportFormat(..),
    ReportResults(..),
    ActivityFilter(..),
    Filter(..),
    Repeater(..),
    defaultFilter,
    defaultReportOptions,
    parseActivityMatcher,
    filterPredicate,
    prepareCalculations,
    processReport,
    processRepeater,
    renderReport
    ) where

import Data.Time
import Data.Maybe
import Data.List
import Data.Ord
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Data.MyText (Text,pack,unpack)
import Data.Function (on)
import System.Locale (defaultTimeLocale)
import Control.Applicative
import Data.Strict ((:!:), Pair(..))
import qualified Data.Strict as Strict
import Data.Traversable (sequenceA)
import Control.Arrow

import Data
import Categorize
import LeftFold
import DumpFormat


data Report = GeneralInfos
    | TotalTime
    | Category Category
    | EachCategory
    | IntervalCategory Category
    | IntervalTag Activity
    | DumpSamples
        deriving (Show, Eq)

data Filter = Exclude ActivityMatcher | Only ActivityMatcher | GeneralCond String
        deriving (Show, Eq)

data ActivityMatcher = MatchActivity Activity | MatchCategory Category
        deriving (Show, Eq)

data ActivityFilter = ExcludeActivity ActivityMatcher | OnlyActivity ActivityMatcher
        deriving (Show, Eq)

data Repeater = ByMinute | ByHour | ByDay | ByMonth | ByYear
        deriving (Show, Eq)

-- Supported report output formats: text, comma-separated values and
-- tab-separated values
data ReportFormat = RFText | RFCSV | RFTSV
        deriving (Show, Eq)

data ReportOptions = ReportOptions
    { roMinPercentage :: Double
    , roReportFormat :: ReportFormat
    , roActivityFilter :: [ActivityFilter]
    }
        deriving (Show, Eq)

defaultReportOptions :: ReportOptions
defaultReportOptions = ReportOptions
    { roMinPercentage = 1
    , roReportFormat = RFText
    , roActivityFilter = []
    }

-- Data format semantically representing the result of a report, including the
-- title
type Interval = (String,String,String,String) 
data ReportResults =
        ListOfFields String [(String, String)]
        | ListOfTimePercValues String [(String, String, Double)]
        | PieChartOfTimePercValues  String [(String, String, Double)]
        | ListOfIntervals String [Interval]
        | MultipleReportResults [ReportResults]
        | RepeatedReportResults String [(String, ReportResults)]
        | DumpResult (TimeLog (CaptureData, TimeZone, ActivityData))


filterPredicate :: [Filter] -> TimeLogEntry (Ctx, ActivityData) -> Bool
filterPredicate filters tl = 
       all (\flag -> case flag of 
                Exclude act  -> excludeTag act tl
                Only act     -> onlyTag act tl
                GeneralCond s-> applyCond s (cTimeZone (fst (tlData tl))) tl) filters

filterActivity :: [ActivityFilter] -> ActivityData -> ActivityData
filterActivity fs = filter (applyActivityFilter fs)

applyActivityFilter :: [ActivityFilter] -> Activity -> Bool
applyActivityFilter fs act = all go fs
    where go (ExcludeActivity matcher) = not (matchActivityMatcher matcher act)
          go (OnlyActivity matcher)    =      matchActivityMatcher matcher act 
                                
excludeTag matcher = not . any (matchActivityMatcher matcher) . snd . tlData
onlyTag matcher = any (matchActivityMatcher matcher) . snd . tlData

defaultFilter :: Filter
defaultFilter = Exclude (MatchActivity inactiveActivity)

matchActivityMatcher :: ActivityMatcher -> Activity -> Bool
matchActivityMatcher (MatchActivity act1) act2 = act1 == act2
matchActivityMatcher (MatchCategory cat) act2 = Just cat == activityCategory act2

parseActivityMatcher :: String -> ActivityMatcher 
parseActivityMatcher str | last str == ':' = MatchCategory (pack (init str))
                         | otherwise       = MatchActivity (read str)

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
        -- , allTags :: TimeLog (Ctx, ActivityData)
        -- tags is a list of uninterrupted entries
        -- , tags :: [TimeLog (Ctx, ActivityData)]
        }

prepareCalculations :: LeftFold (Bool :!: TimeLogEntry (Ctx, ActivityData)) Calculations
prepareCalculations =
    pure (\fd ld ttr tts s -> 
        let c = Calculations
                  { firstDate = fd
                  , lastDate = ld
                  , timeDiff = diffUTCTime (lastDate c) (firstDate c)
                  , totalTimeRec = ttr
                  , totalTimeSel = tts
                  , fractionRec = realToFrac (totalTimeRec c) / (realToFrac (timeDiff c))
                  , fractionSel = realToFrac (totalTimeSel c) / (realToFrac (timeDiff c))
                  , fractionSelRec = realToFrac (totalTimeSel c) / realToFrac (totalTimeRec c)
                  , sums = s
                  } in c) <*>
    onAll calcFirstDate <*>
    onAll calcLastDate <*>
    onAll calcTotalTime <*>
    onSelected calcTotalTime <*>
    onSelected calcSums 
  where

calcFirstDate :: LeftFold (TimeLogEntry a) UTCTime
calcFirstDate = fromJust <$> lfFirst `mapElems` tlTime

calcLastDate :: LeftFold (TimeLogEntry a) UTCTime
calcLastDate = fromJust <$> lfLast `mapElems` tlTime

calcTotalTime :: LeftFold (TimeLogEntry a) NominalDiffTime
calcTotalTime = (/1000) <$> LeftFold 0 (+) fromInteger `mapElems` tlRate

calcSums :: LeftFold (TimeLogEntry (a, [Activity])) (M.Map Activity NominalDiffTime)
calcSums = LeftFold M.empty
            (\m tl ->
                let go' m act = M.insertWith' (+) act (fromInteger (tlRate tl)/1000) m
                in foldl' go' m (snd (tlData tl))) id

processRepeater :: TimeZone -> Repeater -> LeftFold (Bool :!: TimeLogEntry (Ctx, ActivityData)) ReportResults -> LeftFold (Bool :!: TimeLogEntry (Ctx, ActivityData)) ReportResults
processRepeater tz r rep = case repeaterImpl r of
    RepeaterImpl catR showR ->
        filterElems (\(b :!: _) -> b) $
        pure (RepeatedReportResults "Day" . map (first showR) . M.toList) <*>
        multiplex (catR . utcToLocalTime tz . tlTime . Strict.snd) rep

data RepeaterImpl where
  RepeaterImpl :: Ord r => (LocalTime -> r) -> (r -> String) -> RepeaterImpl

repeaterImpl :: Repeater -> RepeaterImpl
repeaterImpl ByMinute = RepeaterImpl
    -- a somewhat lazy implementations, using strings...
    (formatTime defaultTimeLocale "%F %H:%M")
    id
repeaterImpl ByHour = RepeaterImpl
    (formatTime defaultTimeLocale "%F %H")
    id
repeaterImpl ByDay = RepeaterImpl
    localDay
    showGregorian
repeaterImpl ByMonth = RepeaterImpl
    ((\(y,m,_) -> (y, m)) . toGregorian . localDay)
    (\(y,m) -> show y ++ "-" ++ show m)
repeaterImpl ByYear = RepeaterImpl
    ((\(y,_,_) -> y) . toGregorian . localDay)
    show

processReport :: ReportOptions -> Report -> LeftFold (Bool :!: TimeLogEntry (Ctx, ActivityData)) ReportResults
processReport opts GeneralInfos =
   pure (\n firstDate lastDate ttr tts ->
    let timeDiff = diffUTCTime lastDate firstDate
        fractionRec = realToFrac ttr / (realToFrac timeDiff) :: Double
        fractionSel = realToFrac tts / (realToFrac timeDiff) :: Double
        fractionSelRec = realToFrac tts / realToFrac ttr :: Double
    in ListOfFields "General Information"
        [ ("FirstRecord", show firstDate)
        , ("LastRecord",  show lastDate)
        , ("Number of records", show n)
        , ("Total time recorded",  showTimeDiff opts ttr)
        , ("Total time selected",  showTimeDiff opts tts)
        , ("Fraction of total time recorded", printf "%3.0f%%" (fractionRec * 100))
        , ("Fraction of total time selected", printf "%3.0f%%" (fractionSel * 100))
        , ("Fraction of recorded time selected", printf "%3.0f%%" (fractionSelRec * 100))
        ]) <*>
    onAll lfLength <*>
    onAll calcFirstDate <*>
    onAll calcLastDate <*>
    onAll calcTotalTime <*>
    onSelected calcTotalTime

processReport opts  TotalTime =
    onSelected $
        pure (\totalTimeSel sums -> 
            ListOfTimePercValues "Total time per tag" .
            mapMaybe (\(tag,time) ->
                  let perc = realToFrac time/realToFrac totalTimeSel
                      pick = applyActivityFilter (roActivityFilter opts) tag
                  in if pick && perc*100 >= roMinPercentage opts
                  then Just $ ( show tag
                              , showTimeDiff opts time
                              , perc)
                  else Nothing
                  ) .
            reverse .
            sortBy (comparing snd) $
            M.toList $
            sums) <*>
    calcTotalTime <*>
    calcSums 

processReport opts (Category cat) = pure (\c -> processCategoryReport opts c cat) <*>
    prepareCalculations

processReport opts EachCategory = 
    pure (\c cats -> MultipleReportResults $ map (processCategoryReport opts c) cats) <*>
    prepareCalculations <*>
    onSelected calcCategories

processReport opts (IntervalCategory cat) =
    processIntervalReport opts ("Intervals for category " ++ show cat) (extractCat cat) 
    where
        extractCat :: Category -> ActivityData -> Maybe String
        extractCat cat = fmap (unpack . activityName) . listToMaybe . filter ( (==Just cat) . activityCategory )

processReport opts (IntervalTag tag) =
    processIntervalReport opts ("Intervals for category " ++ show tag) (extractTag tag) 
    where
        extractTag :: Activity -> ActivityData -> Maybe String
        extractTag tag = fmap show . listToMaybe . filter ( (==tag) )

processReport opts DumpSamples =
    DumpResult <$> onSelected (mapElems toList $ fmap $
        \(cd,ad) -> (tlData (cNow cd), cTimeZone cd, filterActivity (roActivityFilter opts) ad)
        )

calcCategories :: LeftFold (TimeLogEntry (Ctx, ActivityData)) [Category]
calcCategories = fmap S.toList $ leftFold S.empty $ \s tl ->
    foldl' go' s (snd (tlData tl))
          where go' s (Activity (Just cat) _) = S.insert cat s
                go' s _                       = s

processCategoryReport opts ~(Calculations {..}) cat =
        PieChartOfTimePercValues ("Statistics for category " ++ show cat) $
                let filteredSums = M.filterWithKey (\a _ -> isCategory cat a) sums
                    uncategorizedTime = totalTimeSel - M.fold (+) 0 filteredSums
                    tooSmallSums = M.filter (\t -> realToFrac t / realToFrac totalTimeSel * 100 < roMinPercentage opts) filteredSums
                    tooSmallTimes = M.fold (+) 0 tooSmallSums
                in

                mapMaybe (\(tag,time) ->
                      let perc = realToFrac time/realToFrac totalTimeSel
                          pick = applyActivityFilter (roActivityFilter opts) tag
                      in if pick && perc*100 >= roMinPercentage opts
                      then Just ( show tag
                                , showTimeDiff opts time
                                , perc)
                      else Nothing
                      )
                      (reverse $ sortBy (comparing snd) $ M.toList filteredSums)
                ++
                (
                if tooSmallTimes > 0
                then [( printf "(%d entries omitted)" (M.size tooSmallSums)
                      , showTimeDiff opts tooSmallTimes
                      , realToFrac tooSmallTimes/realToFrac totalTimeSel
                      )]
                else []
                )
                ++      
                (if uncategorizedTime > 0
                then [( "(unmatched time)"
                      , showTimeDiff opts uncategorizedTime
                      , realToFrac uncategorizedTime/realToFrac totalTimeSel
                      )]
                else []
                )

processIntervalReport :: ReportOptions -> String -> (ActivityData -> Maybe String) -> LeftFold (Bool :!: TimeLogEntry (Ctx, ActivityData)) ReportResults
processIntervalReport opts title extr = runOnIntervals  go1 go2
  where
    go1 :: LeftFold (TimeLogEntry (Ctx, ActivityData)) [Interval]
    go1 = go3 `mapElems` fmap (extr . snd) 
    go3 :: LeftFold (TimeLogEntry (Maybe String)) [Interval]
    go3 = runOnGroups ((==) `on` tlData) go4 (onJusts toList)
    go4 :: LeftFold (TimeLogEntry (Maybe String)) (Maybe Interval)
    go4 = pure (\fe le ->
        case tlData fe of
            Just str -> Just
                ( str
                , showUtcTime (tlTime fe)
                , showUtcTime (tlTime le)
                , showTimeDiff opts $
                    tlTime le `diffUTCTime` tlTime fe + fromIntegral (tlRate fe)/1000
                )
            Nothing -> Nothing) <*>
        (fromJust <$> lfFirst) <*>
        (fromJust <$> lfLast)
    go2 :: LeftFold [Interval] ReportResults
    go2 = ListOfIntervals title <$> concatFold
        

{-
        ((extr. snd) `filterWith` 
            runOnIntervals
                (runOnGroups ((==) `on` tlData)
-}


{-
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
-}           
            
renderReport :: ReportOptions -> ReportResults -> IO ()
renderReport opts (DumpResult samples) =
    dumpActivity samples
renderReport opts (MultipleReportResults reports) =
    sequence_ . intersperse (putStrLn "") . map (renderReport opts) $ reports
renderReport opts reportdata =
    putStr $ doRender opts reportdata

doRender :: ReportOptions -> ReportResults -> String
doRender opts reportdata = case roReportFormat opts of
                RFText -> renderReportText id reportdata
                RFCSV -> renderWithDelimiter "," $ renderXSV reportdata
                RFTSV -> renderWithDelimiter "\t" $ renderXSV reportdata

renderReportText titleMod (ListOfFields title dats) = 
    underline (titleMod title) ++
    (tabulate False $ map (\(f,v) -> [f,v]) dats)

renderReportText titleMod (ListOfTimePercValues title dats) = 
    underline (titleMod title) ++ (tabulate True $ listOfValues dats)

renderReportText titleMod (PieChartOfTimePercValues title dats) = 
    underline (titleMod title) ++ (tabulate True $ piechartOfValues dats)

renderReportText titleMod (ListOfIntervals title dats) = 
    underline (titleMod title) ++ (tabulate True $ listOfIntervals dats)

renderReportText titleMod (RepeatedReportResults cat reps) = 
    intercalate "\n" $ map (\(v,rr) -> renderReportText (titleMod . mod v) rr) reps
  where mod v s = s ++ " (" ++ cat ++ " " ++ v ++ ")"

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
renderXSV (ListOfFields title dats) = 
    error ("\"" ++ title ++ "\"" ++ " not supported for this output format")

renderXSV (ListOfTimePercValues _ dats) = listOfValues dats

renderXSV (PieChartOfTimePercValues _ dats) = piechartOfValues dats

renderXSV (ListOfIntervals title dats) = listOfIntervals dats

-- A bit code-smelly here.
renderXSV (RepeatedReportResults cat reps) = title : fields
  where
    title = cat : head (renderXSV (snd (head reps)))
    fields = concatMap (\(v,rr) -> map (v:) (tail (renderXSV rr))) reps

renderWithDelimiter :: String -> [[String]] -> String
renderWithDelimiter delim datasource =
    unlines $ map (intercalate delim) datasource

tabulate :: Bool -> [[String]] -> String
tabulate titlerow rows = unlines $ addTitleRow $ map (intercalate " | " . zipWith (\l s -> take (l - length s) (repeat ' ') ++ s) colwidths) rows
  where cols = transpose rows
        colwidths = map (maximum . map length) cols
        addTitleRow | titlerow  = \(l:ls) -> (map (\c -> if c == ' ' then '_' else c) l ++ "_")
                                             : ls
                 -- | titlerow  = \(l:ls) -> l : (take (length l) (repeat '-')) : ls
                    | otherwise = id

showTimeDiff :: ReportOptions -> NominalDiffTime -> String
showTimeDiff (ReportOptions { roReportFormat = RFText }) = showTimeDiffHuman
showTimeDiff _                                           = showTimeDiffMachine

showTimeDiffHuman :: NominalDiffTime -> String
showTimeDiffHuman t = go False $ zip [days,hours,mins,secs] ["d","h","m","s"]
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

showTimeDiffMachine :: NominalDiffTime -> String
showTimeDiffMachine t = printf "%d:%02d:%02d" hours mins secs
  where s = round t :: Integer
        hours = s `div` (60*60)
        mins  = (s `div` 60) `mod` 60
        secs  =  s `mod` 60 

showUtcTime :: UTCTime -> String
showUtcTime = formatTime defaultTimeLocale "%x %X"

underline :: String -> String
underline str = unlines 
    [ str
    , map (const '=') str
    ]
