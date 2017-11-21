{-# LANGUAGE Rank2Types, CPP #-}
module Categorize where

import Data

import qualified Text.Regex.PCRE.Light.Text as RE
import qualified Data.Map as M
import qualified Data.MyText as T
import Data.MyText (Text)
import Control.Monad
import Control.Monad.Instances
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Functor.Identity


import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.ExprFail
import System.IO
import System.Exit
import Control.Applicative ((<*>),(<$>))
import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (formatTime)
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale, iso8601DateFormat)
#else
import System.Locale (defaultTimeLocale, iso8601DateFormat)
#endif
import Debug.Trace
import Control.Arrow (second)
import Text.Printf

type Categorizer = TimeLog CaptureData -> TimeLog (Ctx, ActivityData)
type Rule = Ctx -> ActivityData

type Parser = ParsecT String () (ReaderT TimeZone Identity)


data Ctx = Ctx
        { cNow :: TimeLogEntry CaptureData
        , cCurrentWindow :: Maybe (Bool, Text, Text)
        , cWindowInScope :: Maybe (Bool, Text, Text)
        , cSubsts :: [Text]
        , cCurrentTime :: UTCTime
        , cTimeZone :: TimeZone
        }
  deriving (Show)

instance NFData Ctx where
    rnf (Ctx a b c d e f) = a `deepseq` b `deepseq` c `deepseq` e `deepseq` e `deepseq` f `deepseq` ()

type Cond = CtxFun [Text]

type CtxFun a = Ctx -> Maybe a

data CondPrim
        = CondString (CtxFun Text)
        | CondRegex (CtxFun RE.Regex)
        | CondInteger (CtxFun Integer)
        | CondTime (CtxFun NominalDiffTime)
        | CondDate (CtxFun UTCTime)
        | CondCond (CtxFun [Text])
        | CondStringList (CtxFun [Text])
        | CondRegexList (CtxFun [RE.Regex])

newtype Cmp = Cmp (forall a. Ord a => a -> a -> Bool)

readCategorizer :: FilePath -> IO Categorizer
readCategorizer filename = do
        h <- openFile filename ReadMode
        hSetEncoding h utf8
        content <- hGetContents h
        time <- getCurrentTime
        tz <- getCurrentTimeZone
        case flip runReader tz $
            runParserT (between (return ()) eof parseRules) () filename content of
          Left err -> do
                putStrLn "Parser error:"
                print err
                exitFailure
          Right cat -> return $
                (map (fmap (mkSecond (postpare . cat))) . prepare time tz)

applyCond :: String -> TimeZone -> TimeLogEntry (Ctx, ActivityData) -> Bool
applyCond s tz = 
        case flip runReader tz $ runParserT (do {c <- parseCond; eof ; return c}) () "commad line parameter" s of
          Left err -> error (show err)
          Right c  -> isJust . c . fst . tlData

prepare :: UTCTime -> TimeZone -> TimeLog CaptureData -> TimeLog Ctx
prepare time tz = map go
  where go now  = now {tlData = Ctx now (findActive (cWindows (tlData now))) Nothing [] time tz }

-- | Here, we filter out tags appearing twice, and make sure that only one of
--   each category survives
postpare :: ActivityData -> ActivityData
postpare = nubBy go
  where go (Activity (Just c1) _) (Activity (Just c2) _) = c1 == c2
        go a1                     a2                     = a1 == a2

lang :: GenTokenParser String () (ReaderT TimeZone Identity)
lang = makeTokenParser $ LanguageDef
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter	 = alphaNum <|> oneOf "_'"
                , opStart	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = True
                }

parseRules :: Parser Rule
parseRules = do 
        whiteSpace lang
        a <- option id (reserved lang "aliases" >> parens lang parseAliasSpecs)
        rb <- parseRulesBody
        return (a . rb)

parseAliasSpecs :: Parser (ActivityData -> ActivityData)
parseAliasSpecs = do as <- sepEndBy1 parseAliasSpec (comma lang)
                     return $ \ad -> foldr doAlias ad as

doAlias :: (Text, Text) -> ActivityData -> ActivityData
doAlias (s1,s2) = map go
  where go (Activity cat tag) = Activity (if cat == Just s1 then Just s2 else cat)
                                         (if tag == s1 then s2 else tag)

parseAliasSpec :: Parser (Text, Text)
parseAliasSpec = do s1 <- T.pack <$> stringLiteral lang
                    reservedOp lang "->"
                    s2 <- T.pack <$> stringLiteral lang
                    return (s1,s2)

parseRulesBody :: Parser Rule
parseRulesBody = do 
        x <- parseRule
        choice [ do comma lang
                    xs <- parseRule `sepEndBy1` comma lang
                    return (matchAny (x:xs))
               , do semi lang
                    xs <- parseRule `sepEndBy1` semi lang
                    return (matchFirst (x:xs))
               ,    return x
               ]

parseRule :: Parser Rule
parseRule = choice
        [    braces lang parseRules
        , do cond <- parseCond
             reservedOp lang "==>"
             rule <- parseRule
             return (ifThenElse cond rule matchNone)
        , do reserved lang "if"
             cond <- parseCond
             reserved lang "then"
             rule1 <- parseRule
             reserved lang "else"
             rule2 <- parseRule
             return (ifThenElse cond rule1 rule2)
        , do reserved lang "tag"
             parseSetTag
        ]

parseCond :: Parser Cond
parseCond = do cp <- parseCondExpr
               case cp of
                CondCond c -> return c
                cp         -> fail $ printf "Expression of type %s" (cpType cp)

parseCondExpr :: Parser CondPrim
parseCondExpr = buildExpressionParser [
                [ Prefix (reservedOp lang "!" >> return checkNot) ],
                [ Prefix (reserved lang "day of week" >> return evalDayOfWeek)
                , Prefix (reserved lang "day of month" >> return evalDayOfMonth)
                , Prefix (reserved lang "month" >> return evalMonth)
                , Prefix (reserved lang "year" >> return evalYear)
                , Prefix (reserved lang "format" >> return formatDate) ],
                [ Infix (reservedOp lang "=~" >> return checkRegex) AssocNone 
                , Infix (checkCmp <$> parseCmp) AssocNone
                ],
                [ Prefix (reserved lang "current window" >> return checkCurrentwindow)
                , Prefix (reserved lang "any window" >> return checkAnyWindow)
                ],
                [ Infix (reservedOp lang "&&" >> return checkAnd) AssocRight ],
                [ Infix (reservedOp lang "||" >> return checkOr) AssocRight ]
            ] parseCondPrim

cpType :: CondPrim -> String
cpType (CondString _) = "String"
cpType (CondRegex _) = "Regex"
cpType (CondInteger _) = "Integer"
cpType (CondTime _) = "Time"
cpType (CondDate _) = "Date"
cpType (CondCond _) = "Condition"
cpType (CondStringList _) = "List of Strings"
cpType (CondRegexList _) = "List of regular expressions"

checkRegex :: CondPrim -> CondPrim -> Erring CondPrim
checkRegex (CondString getStr) (CondRegex getRegex) = Right $ CondCond $ \ctx -> do
        str <- getStr ctx
        regex <- getRegex ctx
        tail <$> RE.match regex str [RE.exec_no_utf8_check]
checkRegex (CondString getStr) (CondRegexList getRegexList) = Right $ CondCond $ \ctx -> do
        str <- getStr ctx
        regexes <- getRegexList ctx
        tail <$> msum (map (\regex -> RE.match regex str [RE.exec_no_utf8_check]) regexes)
checkRegex cp1 cp2 = Left $
        printf "Cannot apply =~ to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkAnd :: CondPrim-> CondPrim -> Erring CondPrim
checkAnd (CondCond c1) (CondCond c2) = Right $ CondCond $ do
        res1 <- c1
        res2 <- c2
        return $ res1 >> res2
checkAnd cp1 cp2 = Left $
        printf "Cannot apply && to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkOr :: CondPrim-> CondPrim -> Erring CondPrim
checkOr (CondCond c1) (CondCond c2) = Right $ CondCond $ do
        res1 <- c1
        res2 <- c2
        return $ res1 `mplus` res2
checkOr cp1 cp2 = Left $
        printf "Cannot apply && to an expression of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkNot :: CondPrim -> Erring CondPrim
checkNot (CondCond getCnd) = Right $ CondCond $ do
        liftM (maybe (Just []) (const Nothing)) getCnd
checkNot cp = Left $
        printf "Cannot apply ! to an expression of type %s"
               (cpType cp)

checkCmp :: Cmp -> CondPrim -> CondPrim -> Erring CondPrim
checkCmp (Cmp (?)) (CondInteger getN1) (CondInteger getN2) = Right $ CondCond $ \ctx -> do
        n1 <- getN1 ctx
        n2 <- getN2 ctx
        guard (n1 ? n2)
        return []
checkCmp (Cmp (?)) (CondTime getT1) (CondTime getT2) = Right $ CondCond $ \ctx -> do
        t1 <- getT1 ctx
        t2 <- getT2 ctx
        guard (t1 ? t2)
        return []
checkCmp (Cmp (?)) (CondDate getT1) (CondDate getT2) = Right $ CondCond $ \ctx -> do
        t1 <- getT1 ctx
        t2 <- getT2 ctx
        guard (t1 ? t2)
        return []
checkCmp (Cmp (?)) (CondString getS1) (CondString getS2) = Right $ CondCond $ \ctx -> do
        s1 <- getS1 ctx
        s2 <- getS2 ctx
        guard (s1 ? s2)
        return []
checkCmp (Cmp (?)) (CondString getS1) (CondStringList getS2) = Right $ CondCond $ \ctx -> do
        s1 <- getS1 ctx
        sl <- getS2 ctx
        guard (any (s1 ?) sl)
        return []
checkCmp _ cp1 cp2 = Left $
        printf "Cannot compare expressions of type %s and type %s"
               (cpType cp1) (cpType cp2)

checkCurrentwindow :: CondPrim -> Erring CondPrim
checkCurrentwindow (CondCond cond) = Right $ CondCond $ \ctx -> 
        cond (ctx { cWindowInScope = cCurrentWindow ctx })
checkCurrentwindow cp = Left $
        printf "Cannot apply current window to an expression of type %s"
               (cpType cp)

checkAnyWindow :: CondPrim -> Erring CondPrim
checkAnyWindow (CondCond cond) = Right $ CondCond $ \ctx ->
        msum $ map (\w -> cond (ctx { cWindowInScope = Just w }))
                                     (cWindows (tlData (cNow ctx)))
checkAnyWindow cp = Left $
        printf "Cannot apply current window to an expression of type %s"
               (cpType cp)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

-- Day of week is an integer in [1..7].
evalDayOfWeek :: CondPrim -> Erring CondPrim
evalDayOfWeek (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = cTimeZone ctx in
  (toInteger . trd3 . toWeekDate . localDay . utcToLocalTime tz) `liftM` df ctx
evalDayOfWeek cp = Left $ printf
  "Cannot apply day of week to an expression of type %s, only to $date."
  (cpType cp)

-- Day of month is an integer in [1..31].
evalDayOfMonth :: CondPrim -> Erring CondPrim
evalDayOfMonth (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = cTimeZone ctx in
  (toInteger . trd3 . toGregorian . localDay . utcToLocalTime tz) `liftM` df ctx
evalDayOfMonth cp = Left $ printf
  "Cannot apply day of month to an expression of type %s, only to $date."
  (cpType cp)

-- Month is an integer in [1..12].
evalMonth :: CondPrim -> Erring CondPrim
evalMonth (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = cTimeZone ctx in
  (toInteger . snd3 . toGregorian . localDay . utcToLocalTime tz) `liftM` df ctx
evalMonth cp = Left $ printf
  "Cannot apply month to an expression of type %s, only to $date."
  (cpType cp)

evalYear :: CondPrim -> Erring CondPrim
evalYear (CondDate df) = Right $ CondInteger $ \ctx ->
  let tz = cTimeZone ctx in
  (fst3 . toGregorian . localDay . utcToLocalTime tz) `liftM` df ctx
evalYear cp = Left $ printf
  "Cannot apply year to an expression of type %s, only to $date."
  (cpType cp)

-- format date according to ISO 8601 (YYYY-MM-DD)
formatDate :: CondPrim -> Erring CondPrim
formatDate (CondDate df) = Right $ CondString $ \ctx ->
  let tz = cTimeZone ctx
      local = utcToLocalTime tz `liftM` df ctx
   in T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing) <$> local
formatDate cp = Left $ printf
  "Cannot format an expression of type %s, only $date." (cpType cp)

parseCmp :: Parser Cmp
parseCmp = choice $ map (\(s,o) -> reservedOp lang s >> return o)
                        [(">=",Cmp (>=)),
                         (">", Cmp (>)),
                         ("==",Cmp (==)),
                         ("=", Cmp (==)),
                         ("!=",Cmp (/=)),
                         ("<", Cmp (<)),
                         ("<=",Cmp (<=))]

parseCondPrim :: Parser CondPrim
parseCondPrim = choice
        [ parens lang parseCondExpr
        , brackets lang (choice [
            (do list <- commaSep1 lang (stringLiteral lang)
                return $ CondStringList (const (Just (map T.pack list)))
            ) <?> "list of strings",
            (do list <- commaSep1 lang parseRegex
                return $ CondRegexList (const (Just list))
            ) <?> "list of regular expressions"
            ])
        , char '$' >> choice 
             [ do backref <- read <$> many1 digit
                  return $ CondString (getBackref backref)
             , do varname <- identifier lang 
                  choice 
                      [ do guard $ varname == "title"
                           return $ CondString (getVar "title")
                      , do guard $ varname == "program"
                           return $ CondString (getVar "program")
                      , do guard $ varname == "active"
                           return $ CondCond checkActive
                      , do guard $ varname == "idle"
                           return $ CondInteger (getNumVar "idle")
                      , do guard $ varname == "time"
                           return $ CondTime (getTimeVar "time")
                      , do guard $ varname == "sampleage"
                           return $ CondTime (getTimeVar "sampleage")
                      , do guard $ varname == "date"
                           return $ CondDate (getDateVar "date")
                      , do guard $ varname == "desktop"
                           return $ CondString (getVar "desktop")
                     ]
              ] <?> "variable"
        , do regex <- parseRegex <?> "regular expression"
             return $ CondRegex (const (Just regex))
        , do str <- T.pack <$> stringLiteral lang <?> "string"
             return $ CondString (const (Just str))
        , try $ do time <- parseTime <?> "time" -- backtrack here, it might have been a number
                   return $ CondTime (const (Just time))
        , try $ do date <- parseDate <?> "date" -- backtrack here, it might have been a number
                   return $ CondDate (const (Just date))
        , do num <- natural lang <?> "number"
             return $ CondInteger (const (Just num))
        ]
{-
                     choice
                        [ do reservedOp lang "=~"
                             regex <- parseRegex
                             return $ checkRegex varname (RE.compile regex [])
                        , do reservedOp lang "==" <|> reservedOp lang "="
                             str <- stringLiteral lang
                             return $ checkEq varname str
                        , do reservedOp lang "/=" <|> reservedOp lang "!="
                             str <- stringLiteral lang
                             return $ checkNot (checkEq varname str)
                        ]
                , do guard $ varname == "idle"
                     op <- parseCmp
                     num <- natural lang
                     return $ checkNumCmp op varname num
                , do guard $ varname `elem` ["time","sampleage"]
                     op <- parseCmp 
                     time <- parseTime
                     return $ checkTimeCmp op varname time
                , do guard $ varname == "active"
                     return checkActive
                ]
        , do reserved lang "current window"
             cond <- parseCond
             return $ checkCurrentwindow cond
        , do reserved lang "any window"
             cond <- parseCond
             return $ checkAnyWindow cond
        ]
-}

parseRegex :: Parser RE.Regex
parseRegex = fmap (flip RE.compile [] . T.pack) $ lexeme lang $ choice
        [ between (char '/') (char '/') (many1 (noneOf "/"))
        , do char 'm'
             c <- anyChar
             str <- many1 (noneOf [c])
             char c
             return str
        ]
             
-- | Parses a day-of-time specification (hh:mm)
parseTime :: Parser NominalDiffTime
parseTime = fmap fromIntegral $ lexeme lang $ do
               hour <- read <$> many1 digit
               char ':'
               minute <- read <$> count 2 digit
               return $ (hour * 60 + minute) * 60

parseDate :: Parser UTCTime
parseDate = lexeme lang $ do
    tz <- lift ask
    year <- read <$> count 4 digit
    char '-'
    month <- read <$> count 2 digit
    char '-'
    day <- read <$> count 2 digit
    time <- option 0 parseTime
    let date = LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)
    return $ addUTCTime time $ localTimeToUTC tz date


parseSetTag :: Parser Rule
parseSetTag = lexeme lang $ do
                 firstPart <- parseTagPart 
                 choice [ do char ':'
                             secondPart <- parseTagPart
                             return $ do cat <- firstPart
                                         tag <- secondPart
                                         return $ maybeToList $ do
                                            cat <- cat
                                            tag <- tag
                                            return $ Activity (Just cat) tag
                        ,    return $ do tag <- firstPart
                                         return $ maybeToList $ do
                                            tag <- tag
                                            return $ Activity Nothing tag
                        ]

replaceForbidden :: Maybe Text -> Maybe Text
replaceForbidden = liftM $ T.map go
  where
    go c | isAlphaNum c  = c
         | c `elem` "-_" = c
         | otherwise     = '_'

parseTagPart :: Parser (Ctx -> Maybe Text)
parseTagPart = do parts <- many1 (choice
                        [ do char '$'
                             (replaceForbidden . ) <$> choice
                               [ do num <- read <$> many1 digit
                                    return $ getBackref num
                               , do varname <- many1 (letter <|> oneOf ".")
                                    return $ getVar varname
                               ] <?> "variable"
                        , do s <- many1 (alphaNum <|> oneOf "-_")
                             return $ const (Just (T.pack s))
                        ])
                  return $ (fmap T.concat . sequence) <$> sequence parts

ifThenElse :: Cond -> Rule -> Rule -> Rule
ifThenElse cond r1 r2 = do res <- cond
                           case res of 
                            Just substs -> r1 . setSubsts substs
                            Nothing -> r2
  where setSubsts :: [Text] -> Ctx -> Ctx
        setSubsts substs ctx = ctx { cSubsts = substs }
        

matchAny :: [Rule] -> Rule
matchAny rules = concat <$> sequence rules
matchFirst :: [Rule] -> Rule
matchFirst rules = takeFirst <$> sequence rules
  where takeFirst [] = []
        takeFirst ([]:xs) = takeFirst xs
        takeFirst (x:xs) = x


getBackref :: Integer -> CtxFun Text
getBackref n ctx = listToMaybe (drop (fromIntegral n-1) (cSubsts ctx))

getVar :: String -> CtxFun Text
getVar v ctx | "current" `isPrefixOf` v = do
                let var = drop (length "current.") v
                win <- cCurrentWindow ctx
                getVar var (ctx { cWindowInScope = Just win })
getVar "title"   ctx = do
                (_,t,_) <- cWindowInScope ctx
                return t
getVar "program" ctx = do
                (_,_,p) <- cWindowInScope ctx
                return p
getVar "desktop" ctx = do
                return $ cDesktop (tlData (cNow ctx))
getVar v ctx = error $ "Unknown variable " ++ v

getNumVar :: String -> CtxFun Integer
getNumVar "idle" ctx = Just $ cLastActivity (tlData (cNow ctx)) `div` 1000

getTimeVar :: String -> CtxFun NominalDiffTime
getTimeVar "time" ctx = Just $
   let utc = tlTime . cNow $ ctx
       tz = cTimeZone ctx
       local = utcToLocalTime tz utc
       midnightUTC = localTimeToUTC tz $ local { localTimeOfDay = midnight }
    in utc `diffUTCTime` midnightUTC
getTimeVar "sampleage" ctx = Just $ cCurrentTime ctx `diffUTCTime` tlTime (cNow ctx)

getDateVar :: String -> CtxFun UTCTime
getDateVar "date" ctx = Just $ tlTime (cNow ctx)

findActive :: [(Bool, t, t1)] -> Maybe (Bool, t, t1)
findActive = find (\(a,_,_) -> a)                                 

checkActive :: Cond
checkActive ctx = do (a,_,_) <- cWindowInScope ctx
                     guard a
                     return []

matchNone :: Rule
matchNone = const []

justIf :: a -> Bool -> Maybe a
justIf x True = Just x
justIf x False = Nothing

mkSecond :: (a -> b) -> a -> (a, b)
mkSecond f a = (a, f a)
