{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden.Manage
import Test.Tasty.Golden
import Test.Tasty.HUnit
import System.Process.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Exception
import Data.Typeable
import System.Exit
import System.Environment

import Categorize
import TimeLog
import Data
import Data.Time.Clock

main = do
    setEnv "TZ" "UTC" -- to make tests reproducible
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests, regressionTests]

regressionTests :: TestTree
regressionTests = testGroup "Regression tests"
    [ testCase "Issue #4" $ do
        cat <- readCategorizer "tests/issue4.cfg"
        let sample = TimeLogEntry undefined 0 (CaptureData [(True, "aa", "program")] 0 "")
        let [TimeLogEntry _ _ (_,acts)] = cat [sample]
        [Activity (Just "Cat") "aa"] @=? acts
        return ()
    , testCase "Issue #5" $ do
        cat <- readCategorizer "tests/issue5.cfg"
        let sample = TimeLogEntry undefined 0 (CaptureData [(True, "aa", "program")] 0 "")
        let [TimeLogEntry _ _ (_,acts)] = cat [sample]
        [Activity Nothing "A2"] @=? acts
        return ()
    , testCase "Issue #14" $ do
        cat <- readCategorizer "tests/issue14.cfg"
        now <- getCurrentTime
        let backThen = (-60*60*101) `addUTCTime` now

        let sample = TimeLogEntry backThen 0 (CaptureData [(True, "aa", "program")] 0 "")
        let [TimeLogEntry _ _ (_,acts)] = cat [sample]
        [Activity Nothing "old"] @=? acts
        return ()
    ]


goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
    [ goldenVsString "dump small"
        "tests/small_dump.out" $
        run "dist/build/arbtt-dump/arbtt-dump" ["-f","tests/small.log", "-t", "Show"] B.empty
    , goldenVsFile "import small"
        "tests/small_import.out" "tests/small_import.out.actual" $ void $
        B.readFile "tests/small_import.in" >>=
        run "dist/build/arbtt-import/arbtt-import" ["-f","tests/small_import.out.actual"]
    , goldenVsFile "recover small"
        "tests/small_borked_recover.out" "tests/small_borked_recover.out.actual" $ void $
        run "dist/build/arbtt-recover/arbtt-recover" ["-i","tests/small_borked_recover.out", "-o", "tests/small_borked_recover.out.actual"] B.empty
    , goldenVsString "stats small"
        "tests/small_stats.out" $
        run "dist/build/arbtt-stats/arbtt-stats" ["--logfile", "tests/small.log", "--categorize", "tests/small.cfg"] B.empty
    , goldenVsString "stats small csv"
        "tests/small_stats_csv.out" $
        run "dist/build/arbtt-stats/arbtt-stats" ["--logfile", "tests/small.log", "--categorize", "tests/small.cfg", "--output-format", "csv"] B.empty
    , goldenVsString "stats small unicode"
        "tests/unicode_stats.out" $
        run "dist/build/arbtt-stats/arbtt-stats" ["--logfile", "tests/unicode.log", "--categorize", "tests/unicode.cfg"] B.empty
    , goldenVsString "stats gap handling"
        "tests/gap-handling.out" $
        run "dist/build/arbtt-stats/arbtt-stats" ["--logfile", "tests/gap-handling.log", "--categorize", "tests/gap-handling.cfg", "--intervals", "Program:"] B.empty
    ]


run :: FilePath -> [FilePath] -> B.ByteString -> IO B.ByteString
run cmd args stdin = do
   (ex,stdout,stderr) <- readProcessWithExitCode cmd args stdin
   unless (B.null stderr) $ throwIO $ StderrException stderr
   case ex of
     ExitSuccess   -> return stdout
     ExitFailure r -> throwIO $ ExitCodeException r

data StderrException = StderrException B.ByteString
     deriving (Show, Typeable)
data ExitCodeException = ExitCodeException Int
     deriving (Show, Typeable)

instance Exception StderrException
instance Exception ExitCodeException
