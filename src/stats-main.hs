module Main where
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.Maybe
import Data.Char (toLower)
import Text.Printf
import Data.Version (showVersion)
import Control.DeepSeq
import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Progress
import System.Posix.Files
import System.ProgressBar
import TermSize
import qualified Data.MyText as T
import Data.Time.LocalTime

import TimeLog
import Categorize
import Stats
import CommonStartup
import LeftFold
import DumpFormat

import Paths_arbtt (version)

data Options = Options
    { optReports :: [Report]
    , optFilters :: [Filter]
    , optRepeater :: [Repeater]
    , optAlsoInactive :: Bool
    , optReportOptions :: ReportOptions
    , optLogFile :: String
    , optCategorizeFile :: String
    }

defaultOptions :: FilePath -> Options
defaultOptions dir = Options
    { optReports = []
    , optFilters = []
    , optRepeater = []
    , optAlsoInactive = False
    , optReportOptions = defaultReportOptions
    , optLogFile = dir </> "capture.log"
    , optCategorizeFile = dir </> "categorize.cfg"
    }

versionStr, header :: String
versionStr = "arbtt-stats " ++ showVersion version
header = "Usage: arbtt-stats [OPTIONS...]"

options :: [OptDescr (Options -> IO Options)]
options =
     [ Option "h?"      ["help"]
              (NoArg $ \_ -> do
                    hPutStr stderr (usageInfo header options)
                    exitSuccess
              )
              "show this help"
     , Option "V"       ["version"]
              (NoArg $ \_ -> do
                    hPutStrLn stderr versionStr
                    exitSuccess
              )
              "show the version number"
--     , Option ['g']     ["graphical"] (NoArg Graphical)    "render the reports as graphical charts"
     , Option ""      ["logfile"]
              (ReqArg (\arg opt -> return opt { optLogFile = arg }) "FILE")
               "use this file instead of ~/.arbtt/capture.log"
     , Option ""      ["categorizefile"]
              (ReqArg (\arg opt -> return opt { optCategorizeFile = arg }) "FILE")
               "use this file instead of ~/.arbtt/categorize.cfg"
     , Option "x"       ["exclude"]
              (ReqArg (\arg opt -> let filters = Exclude (parseActivityMatcher arg) : optFilters opt
                                   in  return opt { optFilters = filters }) "TAG")
              "ignore samples containing this tag or category"
     , Option "o"       ["only"]
              (ReqArg (\arg opt -> let filters = Only (parseActivityMatcher arg) : optFilters opt
                                   in  return opt { optFilters = filters }) "TAG")
              "only consider samples containing this tag or category"
     , Option ""        ["also-inactive"]
              (NoArg (\opt ->      return opt { optAlsoInactive = True }))
              "include samples with the tag \"inactive\""
     , Option "f"       ["filter"]
              (ReqArg (\arg opt -> let filters = GeneralCond arg : optFilters opt
                                   in  return opt { optFilters = filters }) "COND")
              "only consider samples matching the condition"
     , Option "m"       ["min-percentage"]
              (ReqArg (\arg opt -> let ro = (optReportOptions opt) { roMinPercentage = read arg}
                                   in  return opt { optReportOptions = ro }) "PERC")
              "do not show tags with a percentage lower than PERC% (default: 1)"
     , Option ""        ["output-exclude"]
              (ReqArg (\arg opt -> let filters = ExcludeActivity (parseActivityMatcher arg) : roActivityFilter (optReportOptions opt)
                                   in  return opt { optReportOptions = (optReportOptions opt) { roActivityFilter = filters }}) "TAG")
              "remove these tags from the output"
     , Option ""        ["output-only"]
              (ReqArg (\arg opt -> let filters = OnlyActivity (parseActivityMatcher arg) : roActivityFilter (optReportOptions opt)
                                   in  return opt { optReportOptions = (optReportOptions opt) { roActivityFilter = filters }}) "TAG")
              "only include these tags in the output"
     , Option "i"       ["information"]
              (NoArg (\opt ->      let reports = GeneralInfos : optReports opt
                                   in  return opt { optReports = reports }))
              "show general statistics about the data"
     , Option "t"       ["total-time"]
              (NoArg (\opt ->      let reports = TotalTime : optReports opt
                                   in  return opt { optReports = reports }))
              "show total time for each tag"
     , Option "c"       ["category"]
              (ReqArg (\arg opt -> let reports = Category (T.pack arg) : optReports opt
                                   in  return opt { optReports = reports }) "CATEGORY")
              "show statistics about category CATEGORY"
     , Option ""        ["each-category"]
              (NoArg (\opt ->      let reports = EachCategory : optReports opt
                                   in  return opt { optReports = reports }))
              "show statistics about each category found"
     , Option ""        ["intervals"]
              (ReqArg (\arg opt -> let report = if last arg == ':'
                                                then IntervalCategory (T.pack (init arg))
                                                else IntervalTag (read arg)
                                       reports = report : optReports opt
                                   in  return opt { optReports = reports }) "TAG")
              "list intervals of tag or category TAG"
     , Option ""       ["dump-samples"]
              (NoArg (\opt ->      let reports = DumpSamples : optReports opt
                                   in  return opt { optReports = reports }))
              "Dump the raw samples and tags."
     , Option ""       ["output-format"]
              (ReqArg (\arg opt -> let ro = (optReportOptions opt) { roReportFormat = readReportFormat arg }
                                   in  return opt { optReportOptions = ro }) "FORMAT")
              "one of: text, csv (comma-separated values), tsv (TAB-separated values) (default: Text)"
     , Option ""       ["for-each"]
              (ReqArg (\arg opt -> let repeater = readRepeater arg : optRepeater opt
                                   in  return opt { optRepeater = repeater }) "PERIOD")
              "one of: day, month, year"
     ]

readRepeater :: String -> Repeater
readRepeater arg =
    case map toLower arg of
        "minute" -> ByMinute
        "hour"   -> ByHour
        "day"    -> ByDay
        "month"  -> ByMonth
        "year"   -> ByYear
        _        -> error ("Unsupported parameter to --for-each: '" ++ arg ++ "'")

readReportFormat :: String -> ReportFormat
readReportFormat arg =
    case map toLower arg of
        "text" -> RFText
        "csv"  -> RFCSV
        "tsv"  -> RFTSV
        _      -> error ("Unsupported report output format: '" ++ arg ++ "'")

main :: IO ()
main = do
  commonStartup
  args <- getArgs
  actions <- case getOpt Permute options args of
          (o,[],[])  -> return o
          (_,_,errs) -> do
                hPutStr stderr (concat errs ++ usageInfo header options)
                exitFailure
  tz <- getCurrentTimeZone

  dir <- getAppUserDataDirectory "arbtt"
  flags <- foldl (>>=) (return (defaultOptions dir)) actions

  fileEx <- doesFileExist (optCategorizeFile flags)
  unless fileEx $ do
     putStrLn $ printf "Configuration file %s does not exist." (optCategorizeFile flags)
     putStrLn "Please see the example file and the README for more details"
     exitFailure
  categorizer <- readCategorizer (optCategorizeFile flags)

  timelog <- BS.readFile (optLogFile flags)
  isTerm <- hIsTerminalDevice stderr

  trackedTimelog <- case isTerm of
    True -> do
      hSetBuffering stderr NoBuffering
      size <- fileSize <$> getFileStatus (optLogFile flags)
      trackProgressWithChunkSize (fromIntegral size `div` 100) (\_ b -> do
        (_height, width) <- getTermSize
        hPutChar stderr '\r'
        hPutStr stderr $
            mkProgressBar (msg "Processing data") percentage (fromIntegral width) (fromIntegral b) (fromIntegral size)
        when  (fromIntegral b >= fromIntegral size) $ do
            hPutChar stderr '\r'
            hPutStr stderr (replicate width ' ')
            hPutChar stderr '\r'
        ) timelog
    False -> return timelog

  let captures = parseTimeLog trackedTimelog
  let allTags = categorizer captures

  when (null allTags) $ do
     putStrLn "Nothing recorded yet"
     exitFailure

  let filters = (if optAlsoInactive flags then id else (defaultFilter:)) $ optFilters flags

  let rep = case optReports flags of
                [] -> TotalTime
                [x] -> x
                _ -> error "Please specify exactly one report to generate"
  let repeater = foldr (.) id $ map (processRepeater tz) (optRepeater flags)

  let opts = optReportOptions flags
  let fold = filterPredicate filters `adjoin` repeater (processReport opts rep)
  let result = runLeftFold fold allTags

  -- Force the results a bit, to ensure the progress bar to be shown before the title
  result `seq` return ()

  renderReport opts result

{-
import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

        graphicalReport TotalTime = do
          let values = zipWith (\(k,v) n -> (PlotIndex n,[fromIntegral v::Double])) (M.toList sums) [1..]
          let plot = plot_bars_values ^= values $ defaultPlotBars
          let layoutaxis = laxis_generate ^= autoIndexAxis (map (show.fst) (M.toList  sums)) $
                           defaultLayoutAxis
          let layout = layout1_plots ^= [Right (plotBars plot)] $
                       layout1_bottom_axis ^= layoutaxis $
                       defaultLayout1
          do renderableToWindow (toRenderable layout) 800 600
-}
