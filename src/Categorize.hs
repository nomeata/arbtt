module Categorize where

import Data

import Data.Maybe
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

type Categorizer = TimeLog CaptureData -> TimeLog ActivityData
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

type Cond = Ctx -> Maybe [String]

readCategorizer :: FilePath -> IO Categorizer
readCategorizer filename = do
	content <- readFile filename
	time <- getCurrentTime
	case parse (do {r <- parseRules; eof ; return r}) filename content of
	  Left err -> do
	  	putStrLn "Parser error:"
		putStrLn (show err)
		exitFailure
	  Right cat -> return ((fmap . fmap) (postpare . cat) . prepare time)

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
postpare = nubBy $ go
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

parseRulesBody :: Parser (Ctx -> ActivityData)
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
parseCond = buildExpressionParser [
		[ Prefix (reservedOp lang "!" >> return checkNot) ],
		[ Infix (reservedOp lang "&&" >> return checkAnd) AssocLeft ],
		[ Infix (reservedOp lang "||" >> return checkOr) AssocLeft ]
	    ] parseCondPrim

checkAnd :: Cond -> Cond -> Cond	    
checkAnd c1 c2 = do res1 <- c1
                    res2 <- c2
		    return $ res1 >> res2


checkOr :: Cond -> Cond -> Cond	    
checkOr c1 c2 = do res1 <- c1
                   res2 <- c2
		   return $ res1 `mplus` res2

checkNot :: Cond -> Cond
checkNot = liftM (maybe (Just []) (const Nothing))

parseCmp :: Ord a => Parser (a -> a -> Bool)
parseCmp = choice $ map (\(s,o) -> reservedOp lang s >> return o)
		     	[(">=",(>=)),
			 (">", (>)),
			 ("=", (==)),
			 ("==",(==)),
			 ("<",(<)),
			 ("<=",(<=))]


parseCondPrim :: Parser Cond
parseCondPrim = choice
	[    parens lang parseCond
	, do char '$'
	     varname <- show `liftM` natural lang <|> identifier lang
	     choice
	     	[ do guard $ varname `elem` ["title","program"]
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
		, do guard $ varname == "time"
		     op <- parseCmp 
		     time <- parseTime
		     return $ checkTimeCmp op varname time
		, do guard $ varname == "active"
		     return $ checkActive
		]
	, do reserved lang "current window"
	     cond <- parseCond
	     return $ checkCurrentwindow cond
	, do reserved lang "any window"
	     cond <- parseCond
	     return $ checkAnyWindow cond
	]

parseRegex :: Parser String
parseRegex = lexeme lang $ choice
	[ between (char '/') (char '/') (many1 (noneOf "/"))
	, do char 'm'
	     c <- anyChar
	     str <- many1 (noneOf [c])
	     char c
	     return str
	]
	     
-- | Parses a day-of-time specification (hh:mm) to the number of seconds since
-- 00:00
parseTime :: Parser Integer
parseTime = fmap fromIntegral $ lexeme lang $ do
               h <- digitToInt <$> digit
	       mh <- optionMaybe (do{ digitToInt <$> digit })
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
			     varname <- many1 (letter <|> oneOf ".") <|> many1 digit
			     return $ getVar varname
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

getVar :: String -> Ctx -> Maybe String
getVar v ctx | all isNumber  v = 
		let n = read v in
		listToMaybe (drop (n-1) (cSubsts ctx))
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

checkRegex :: String -> RE.Regex -> Cond
checkRegex varname regex ctx = do s <- getVar varname ctx
		                  matches <- RE.match regex s []
				  return (tail matches)

checkEq :: String -> String -> Cond
checkEq varname str ctx = do s <- getVar varname ctx
		             [] `justIf` (s == str)

findActive :: [(Bool, t, t1)] -> Maybe (Bool, t, t1)
findActive = find (\(a,_,_) -> a)				  

checkCurrentwindow :: Cond -> Cond
checkCurrentwindow cond ctx = cond (ctx { cWindowInScope = findActive (cWindows (tlData (cNow ctx))) })

checkAnyWindow :: Cond -> Cond
checkAnyWindow cond ctx = msum $ map (\w -> cond (ctx { cWindowInScope = Just w }))
                                     (cWindows (tlData (cNow ctx)))

checkActive :: Cond
checkActive ctx = do (a,_,_) <- cWindowInScope ctx
                     guard a
		     return []

checkNumCmp ::  (Integer -> Integer -> Bool) -> String -> Integer -> Cond
checkNumCmp (<?>) "idle" num ctx = [] `justIf` (cLastActivity (tlData (cNow ctx)) <?> (num*1000))

checkTimeCmp ::  (DiffTime -> DiffTime -> Bool) -> String -> Integer -> Cond
checkTimeCmp (<?>) "time" num ctx =
	let time = utctDayTime (tlTime (cNow ctx))
	in [] `justIf` (time <?> secondsToDiffTime num)

matchNone :: Rule
matchNone = const []

justIf :: a -> Bool -> Maybe a
justIf x True = Just x
justIf x False = Nothing
