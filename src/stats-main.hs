module Main where
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import qualified Data.MyText as T
import Data.Char (toLower)
import Text.Printf
import Data.Version (showVersion)

import TimeLog
import Categorize
import Stats
import CommonStartup

import Paths_arbtt (version)

data Options = Options
    { optReports :: [Report]
    , optFilters :: [Filter]
    , optAlsoInactive :: Bool
    , optReportOptions :: ReportOptions
    , optLogFile :: String
    , optCategorizeFile :: String
    }

defaultOptions :: FilePath -> Options
defaultOptions dir = Options
    { optReports = []
    , optFilters = []
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
              (ReqArg (\arg opt -> let filters = Exclude (read arg) : optFilters opt
                                   in  return opt { optFilters = filters }) "TAG")
              "ignore samples containing this tag"
     , Option "o"       ["only"]
              (ReqArg (\arg opt -> let filters = Only (read arg) : optFilters opt
                                   in  return opt { optFilters = filters }) "TAG")
              "only consider samples containing this tag"
     , Option ""        ["also-inactive"]
              (NoArg (\opt ->      return opt { optAlsoInactive = True }))
              "include samples with the tag \"inactive\""
     , Option "f"       ["filter"]
              (ReqArg (\arg opt -> let filters = GeneralCond arg : optFilters opt
                                   in  return opt { optFilters = filters }) "COND")
              "only consider samples matching the condition"
     , Option "m"       ["min-percentage"]
              (ReqArg (\arg opt -> let ro = (optReportOptions opt) { roMinPercentage = read arg}
                                   in  return opt { optReportOptions = ro }) "COND")
              "do not show tags with a percentage lower than PERC% (default: 1)"
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
     , Option ""       ["output-format"]
              (ReqArg (\arg opt -> let ro = (optReportOptions opt) { roReportFormat = readReportFormat arg }
                                   in  return opt { optReportOptions = ro }) "FORMAT")
              "one of: text, csv (comma-separated values), tsv (TAB-separated values) (default: Text)"
     ]

readReportFormat :: String -> ReportFormat
readReportFormat arg =
    case (tolower arg) of
        "text" -> RFText
        "csv"  -> RFCSV
        "tsv"  -> RFTSV
        _      -> error ("Unsupported report output format: '" ++ arg ++ "'")
    where
        tolower = map toLower

main :: IO ()
main = do
  commonStartup
  args <- getArgs
  actions <- case getOpt Permute options args of
          (o,[],[])  -> return o
          (_,_,errs) -> do
                hPutStr stderr (concat errs ++ usageInfo header options)
                exitFailure

  dir <- getAppUserDataDirectory "arbtt"
  flags <- foldl (>>=) (return (defaultOptions dir)) actions

  fileEx <- doesFileExist (optCategorizeFile flags)
  unless fileEx $ do
     putStrLn $ printf "Configuration file %s does not exist." (optCategorizeFile flags)
     putStrLn "Please see the example file and the README for more details"
     exitFailure
  categorizer <- readCategorizer (optCategorizeFile flags)

  captures <- readTimeLog (optLogFile flags)
  let allTags = categorizer captures
  when (null allTags) $ do
     putStrLn "Nothing recorded yet"
     exitFailure
      
  let filters = (if optAlsoInactive flags then id else (defaultFilter:)) $ optFilters flags
  let tags = applyFilters filters allTags
  let reps = case optReports flags of {[] -> [TotalTime]; reps -> reps }

  -- These are defined here, but of course only evaluated when any report
  -- refers to them. Some are needed by more than one report, which is then
  -- advantageous.
  let c = prepareCalculations allTags tags
  
  putReports (optReportOptions flags) c reps

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
