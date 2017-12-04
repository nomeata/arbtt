{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleContexts #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Lazy as B
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Time.LocalTime (getCurrentTimeZone)
import           Data.Typeable
import           System.Environment
import           System.Exit
import           System.Process.ByteString.Lazy
import           Test.Tasty hiding (defaultMain)
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Manage
import           Test.Tasty.HUnit
import           Text.Parsec

import           Categorize
import           TimeLog
import           Data
import           Data.Time.Clock

main = do
    setEnv "TZ" "UTC" -- to make tests reproducible
    distDir <- fromMaybe "dist" `liftM` lookupEnv "HASKELL_DIST_DIR"
    defaultMain (tests distDir)

tests :: FilePath -> TestTree
tests distDir = testGroup "Tests" [goldenTests distDir, regressionTests, parserTests]

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


goldenTests :: FilePath -> TestTree
goldenTests distDir = testGroup "Golden tests"
    [ goldenVsString "dump small"
        "tests/small_dump.out" $
        run (distDir ++ "/build/arbtt-dump/arbtt-dump") ["-f","tests/small.log", "-t", "Show"] B.empty
    , goldenVsFile "import small"
        "tests/small_import.out" "tests/small_import.out.actual" $ void $
        B.readFile "tests/small_import.in" >>=
        run (distDir ++ "/build/arbtt-import/arbtt-import") ["-f","tests/small_import.out.actual"]
    , goldenVsFile "recover small"
        "tests/small_borked_recover.out" "tests/small_borked_recover.out.actual" $ void $
        run (distDir ++ "/build/arbtt-recover/arbtt-recover") ["-i","tests/small_borked_recover.out", "-o", "tests/small_borked_recover.out.actual"] B.empty
    , goldenVsString "stats small"
        "tests/small_stats.out" $
        run (distDir ++ "/build/arbtt-stats/arbtt-stats") ["--logfile", "tests/small.log", "--categorize", "tests/small.cfg"] B.empty
    , goldenVsString "stats small csv"
        "tests/small_stats_csv.out" $
        run (distDir ++ "/build/arbtt-stats/arbtt-stats") ["--logfile", "tests/small.log", "--categorize", "tests/small.cfg", "--output-format", "csv"] B.empty
    , goldenVsString "stats small unicode"
        "tests/unicode_stats.out" $
        run (distDir ++ "/build/arbtt-stats/arbtt-stats") ["--logfile", "tests/unicode.log", "--categorize", "tests/unicode.cfg"] B.empty
    , goldenVsString "stats gap handling"
        "tests/gap-handling.out" $
        run (distDir ++ "/build/arbtt-stats/arbtt-stats") ["--logfile", "tests/gap-handling.log", "--categorize", "tests/gap-handling.cfg", "--intervals", "Program:"] B.empty
    , goldenVsString "condition binding stats"
        "tests/condition_bindings_stats.out" $
        run (distDir ++ "/build/arbtt-stats/arbtt-stats") ["--logfile", "tests/small.log", "--categorize", "tests/condition_bindings.cfg"] B.empty
    ]

testParser env parser input = do
  tz <- getCurrentTimeZone
  return . runIdentity . flip runReaderT (tz, env) . runParserT parser () "" $ input

parserTests :: TestTree
parserTests = testGroup "Parser tests"
    [ testCase "Parse condition bindings" $ do
        result <- testParser Map.empty parseConditionBinding ("condition foo = " ++ condText ++ " in 1 == 1 ==> tag foo")
        assertRight result
    , testCase "Trying to bind reserved identifiers" $ do
        result <- testParser Map.empty parseConditionBinding ("condition title = " ++ condText)
        assertLeft result
    , testCase "Reference bound condition identifiers" $ do
        Right cond <- testParser Map.empty parseCond condText
        result <- testParser (Map.fromList [("foo", cond)]) parseRule ("$foo ==> tag sometag")
        assertRight result
    , testCase "Parse condition binding usage in rule" $ do
        result <- testParser Map.empty parseRules ("condition foo = " ++ condText ++ " in " ++ ruleText)
        assertRight result
    , testCase "Reference unbound condition identifier" $ do
        result <- testParser Map.empty parseRule ("$foo ==> tag sometag")
        assertLeft result
    , testCase "Variables are only accessible within the condition assignment body" $ do
        result <- testParser Map.empty parseRules ("condition foo = " ++ condText ++ " in "
                                                ++ ruleText ++ ", "
                                                ++ ruleText)
        assertLeft result
    ]
  where condText = "current window $title == \"test\""
        ruleText = "$foo ==> tag sometag"

run :: FilePath -> [FilePath] -> B.ByteString -> IO B.ByteString
run cmd args stdin = do
   (ex,stdout,stderr) <- readProcessWithExitCode cmd args stdin
   unless (B.null stderr) $ throwIO $ StderrException stderr
   case ex of
     ExitSuccess   -> return (B.filter (not . (==13)) stdout) -- windows compat
     ExitFailure r -> throwIO $ ExitCodeException r

data StderrException = StderrException B.ByteString
     deriving (Show, Typeable)
data ExitCodeException = ExitCodeException Int
     deriving (Show, Typeable)

instance Exception StderrException
instance Exception ExitCodeException

assertRight :: Show a => Either a b -> Assertion
assertRight (Left r) = assertFailure $ "expected a Right\n but got a Left with: " ++ show r
assertRight _ = return ()

assertLeft :: Show b => Either a b -> Assertion
assertLeft (Right r) = assertFailure $ "expected a Left\n but got a Right with: " ++ show r
assertLeft _ = return ()
