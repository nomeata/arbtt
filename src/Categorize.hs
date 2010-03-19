{-# LANGUAGE Rank2Types #-}
module Categorize where

import Data

import qualified Text.Regex.PCRE.Light.Char8 as RE
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Instances

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import System.Exit
import Control.Applicative ((<*>),(<$>))
import Data.List
import Data.Maybe
import Data.Char
import Data.Time.Clock
import Debug.Trace
import Control.Arrow (second)
import Text.Printf

type Categorizer = TimeLog CaptureData -> TimeLog (Ctx, ActivityData)
type Rule = Ctx -> ActivityData

type Parser a = CharParser () a

data Ctx = Ctx
	{ cNow :: TimeLogEntry CaptureData
	, cPast :: [TimeLogEntry CaptureData]
	, cFuture :: [TimeLogEntry CaptureData]
	, cWindowInScope :: Maybe (Bool, String, String)
	, cSubsts :: [String]
	, cCurrentTime :: UTCTime
	}
  deriving (Show)

type Cond = CtxFun [String]

type CtxFun a = Ctx -> Maybe a

data CondPrim
	= CondString (CtxFun String)
	| CondRegex (CtxFun RE.Regex)
	| CondInteger (CtxFun Integer)
	| CondTime (CtxFun NominalDiffTime)
	| CondCond (CtxFun [String])

newtype Cmp = Cmp (forall a. Ord a => a -> a -> Bool)

readCategorizer :: FilePath -> IO Categorizer
readCategorizer filename = do
	content <- readFile filename
	time <- getCurrentTime
	case parse (do {r <- parseRules; eof ; return r}) filename content of
	  Left err -> do
	  	putStrLn "Parser error:"
		print err
		exitFailure
	  Right cat -> return ((fmap . fmap) (mkSecond (postpare . cat)) . prepare time)

applyCond :: String -> TimeLog (Ctx, ActivityData) -> TimeLog (Ctx, ActivityData)
applyCond s = 
	case parse (do {c <- parseCond; eof ; return c}) "commad line parameter" s of
	  Left err -> error (show err)
	  Right c    -> filter (isJust . c . fst . tlData)

prepare :: UTCTime -> TimeLog CaptureData -> TimeLog Ctx
prepare time tl = go' [] tl tl
  where go' past [] []
  		= []
        go' past (this:future) (now:rest)
	        = now {tlData = Ctx now past future Nothing [] time } :
	          go' (this:past) future rest

-- | Here, we filter out tags appearing twice, and make sure that only one of
--   each category survives
postpare :: ActivityData -> ActivityData
postpare = nubBy go
  where go (Activity (Just c1) _) (Activity (Just c2) _) = c1 == c2
        go a1                     a2                     = a1 == a2

lang :: TokenParser ()
lang = haskell

parseRules :: Parser Rule
parseRules = do 
	whiteSpace lang
	a <- option id (reserved lang "aliases" >> parens lang parseAliasSpecs)
	rb <- parseRulesBody
	return (a . rb)

parseAliasSpecs :: Parser (ActivityData -> ActivityData)
parseAliasSpecs = do as <- sepEndBy1 parseAliasSpec (comma lang)
		     return $ \ad -> foldr doAlias ad as

doAlias :: (String, String) -> ActivityData -> ActivityData
doAlias (s1,s2) = map go
  where go (Activity cat tag) = Activity (if cat == Just s1 then Just s2 else cat)
                                         (if tag == s1 then s2 else tag)

parseAliasSpec :: Parser (String, String)
parseAliasSpec = do s1 <- stringLiteral lang
                    reservedOp lang "->"
		    s2 <- stringLiteral lang
		    return (s1,s2)

parseRulesBody :: Parser Rule
parseRulesBody = do 
	x <- parseRule
	choice [ do comma lang
		    xs <- sepEndBy1 parseRule (comma lang)
		    return (matchAny (x:xs))
	       , do semi lang
	            xs <- many1 (semi lang >> parseRule)
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
		cp         -> unexpected $ printf "Expression of type %s" (cpType cp)

parseCondExpr :: Parser CondPrim
parseCondExpr  = buildExpressionParser [
		[ Prefix (reservedOp lang "!" >> return checkNot) ],
		[ Infix (reservedOp lang "=~" >> return checkRegex) AssocNone 
		, Infix (checkCmp <$> parseCmp) AssocNone
		],
		[ Prefix (reserved lang "current window" >> return checkCurrentwindow)
		, Prefix (reserved lang "any window" >> return checkAnyWindow)
		],
		[ Infix (reservedOp lang "&&" >> return checkAnd) AssocLeft ],
		[ Infix (reservedOp lang "||" >> return checkOr) AssocLeft ]
	    ] parseCondPrim

cpType :: CondPrim -> String
cpType (CondString _) = "String"
cpType (CondRegex _) = "Regex"
cpType (CondInteger _) = "Integer"
cpType (CondTime _) = "Time"
cpType (CondCond _) = "Condition"

checkRegex :: CondPrim -> CondPrim -> CondPrim
checkRegex (CondString getStr) (CondRegex getRegex) = CondCond $ \ctx -> do
	str <- getStr ctx
	regex <- getRegex ctx
	tail <$> RE.match regex str []
checkRegex cp1 cp2 = error $
	printf "Can not apply =~ to an expression of type %s and type %s"
	       (cpType cp1) (cpType cp2)

checkAnd :: CondPrim-> CondPrim -> CondPrim
checkAnd (CondCond c1) (CondCond c2) = CondCond $ do
	res1 <- c1
        res2 <- c2
	return $ res1 >> res2
checkAnd cp1 cp2 = error $
	printf "Can not apply && to an expression of type %s and type %s"
	       (cpType cp1) (cpType cp2)

checkOr :: CondPrim-> CondPrim -> CondPrim
checkOr (CondCond c1) (CondCond c2) = CondCond $ do
	res1 <- c1
        res2 <- c2
	return $ res1 `mplus` res2
checkOr cp1 cp2 = error $
	printf "Can not apply && to an expression of type %s and type %s"
	       (cpType cp1) (cpType cp2)

checkNot :: CondPrim -> CondPrim
checkNot (CondCond getCnd) = CondCond $ do
	liftM (maybe (Just []) (const Nothing)) getCnd
checkNot cp = error $
	printf "Can not apply ! to an expression of type %s"
	       (cpType cp)

checkCmp :: Cmp -> CondPrim -> CondPrim -> CondPrim
checkCmp (Cmp (?)) (CondInteger getN1) (CondInteger getN2) = CondCond $ \ctx -> do
	n1 <- getN1 ctx
	n2 <- getN2 ctx
	guard (n1 ? n2)
	return []
checkCmp (Cmp (?)) (CondTime getT1) (CondTime getT2) = CondCond $ \ctx -> do
	t1 <- getT1 ctx
	t2 <- getT2 ctx
	guard (t1 ? t2)
	return []
checkCmp (Cmp (?)) (CondString getS1) (CondString getS2) = CondCond $ \ctx -> do
	s1 <- getS1 ctx
	s2 <- getS2 ctx
	guard (s1 ? s2)
	return []
checkCmp _ cp1 cp2 = error $
	printf "Can not compare expressions of type %s and type %s"
	       (cpType cp1) (cpType cp2)

checkCurrentwindow :: CondPrim -> CondPrim
checkCurrentwindow (CondCond cond) = CondCond $ \ctx -> 
	cond (ctx { cWindowInScope = findActive (cWindows (tlData (cNow ctx))) })
checkCurrentwindow cp = error $
	printf "Can not apply current window to an expression of type %s"
	       (cpType cp)

checkAnyWindow :: CondPrim -> CondPrim
checkAnyWindow (CondCond cond) = CondCond $ \ctx ->
	msum $ map (\w -> cond (ctx { cWindowInScope = Just w }))
                                     (cWindows (tlData (cNow ctx)))
checkAnyWindow cp = error $
	printf "Can not apply current window to an expression of type %s"
	       (cpType cp)

parseCmp :: Parser Cmp
parseCmp = choice $ map (\(s,o) -> reservedOp lang s >> return o)
		     	[(">=",Cmp (>=)),
			 (">", Cmp (>)),
			 ("==",Cmp (==)),
			 ("=", Cmp (==)),
			 ("<", Cmp (<)),
			 ("<=",Cmp (<=))]

parseCondPrim :: Parser CondPrim
parseCondPrim = choice
	[ parens lang parseCondExpr
	, char '$' >> choice 
	     [ do backref <- natural lang
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
		     ]
	      ] <?> "variable"
	, do regex <- parseRegex <?> "regular expression"
	     return $ CondRegex (const (Just regex))
	, do str <- stringLiteral lang <?> "string"
	     return $ CondString (const (Just str))
	, try $ do time <- parseTime <?> "time" -- backtrack here, it might have been a number
	           return $ CondTime (const (Just time))
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
parseRegex = fmap (flip RE.compile []) $ lexeme lang $ choice
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
               h <- digitToInt <$> digit
	       mh <- optionMaybe (digitToInt <$> digit)
	       char ':'
               m1 <- digitToInt <$> digit
               m2 <- digitToInt <$> digit
	       let hour = maybe h ((10*h)+) mh
	       return $ (hour * 60 + m1 * 10 + m2) * 60

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

parseTagPart :: Parser (Ctx -> Maybe String)
parseTagPart = do parts <- many1 (choice 
			[ do char '$'
			     choice 
			       [ do num <- natural lang
			  	    return $ getBackref num
			       , do varname <- many1 (letter <|> oneOf ".") 
			            return $ getVar varname
			       ] <?> "variable"
			, do s <- many1 (letter <|> oneOf "-_")
			     return $ const (Just s)
			])
		  return $ (fmap concat . sequence) <$> sequence parts

ifThenElse :: Cond -> Rule -> Rule -> Rule
ifThenElse cond r1 r2 = do res <- cond
                           case res of 
			    Just substs -> r1 . setSubsts substs
			    Nothing -> r2
  where setSubsts :: [String] -> Ctx -> Ctx
        setSubsts substs ctx = ctx { cSubsts = substs }
	

matchAny :: [Rule] -> Rule
matchAny rules = concat <$> sequence rules
matchFirst :: [Rule] -> Rule
matchFirst rules = takeFirst <$> sequence rules
  where takeFirst [] = []
        takeFirst ([]:xs) = takeFirst xs
	takeFirst (x:xs) = x


getBackref :: Integer -> CtxFun String
getBackref n ctx = listToMaybe (drop (fromIntegral n-1) (cSubsts ctx))

getVar :: String -> CtxFun String
getVar v ctx | "current" `isPrefixOf` v = do
		let var = drop (length "current.") v
		win <- findActive $ cWindows (tlData (cNow ctx))
		getVar var (ctx { cWindowInScope = Just win })
getVar "title"   ctx = do
		(_,t,_) <- cWindowInScope ctx
                return t
getVar "program" ctx = do
		(_,_,p) <- cWindowInScope ctx
                return p
getVar v ctx = error $ "Unknown variable " ++ v

getNumVar :: String -> CtxFun Integer
getNumVar "idle" ctx = Just $ cLastActivity (tlData (cNow ctx)) `div` 1000

getTimeVar :: String -> CtxFun NominalDiffTime
getTimeVar "time" ctx = Just $ tlTime (cNow ctx) `diffUTCTime` (tlTime (cNow ctx)) { utctDayTime = fromIntegral 0}
getTimeVar "sampleage" ctx = Just $ cCurrentTime ctx `diffUTCTime` tlTime (cNow ctx)


checkEq :: String -> String -> Cond
checkEq varname str ctx = do s <- getVar varname ctx
		             [] `justIf` (s == str)

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
