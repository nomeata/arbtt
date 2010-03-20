--------------------------------------------------------------------
-- |
-- Module   : Text.Regex.PCRE.Light.Char8
-- Copyright: Copyright (c) 2007-2008, Don Stewart
-- License  : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  experimental
-- Portability: H98 + FFI
--
--------------------------------------------------------------------
-- 
-- A simple, portable binding to perl-compatible regular expressions
-- (PCRE) via 8-bit latin1 Strings.
--

module Text.Regex.PCRE.Light.Char8 (

        -- * The abstract PCRE Regex type
          Regex

        -- * String interface
        , compile, compileM
        , match

        -- * Regex types and constructors externally visible

        -- ** PCRE compile-time bit flags
        , PCREOption

        , anchored
        , auto_callout
        {-, bsr_anycrlf-}
        {-, bsr_unicode-}
        , caseless
        , dollar_endonly
        , dotall
        , dupnames
        , extended
        , extra
        , firstline
        , multiline
        {-, newline_any-}
        {-, newline_anycrlf-}
        , newline_cr
        , newline_crlf
        , newline_lf
        , no_auto_capture
        , ungreedy
        , utf8
        , no_utf8_check

        -- ** PCRE exec-time bit flags
        , PCREExecOption

        , exec_anchored
        {-, exec_newline_any     -}
        {-, exec_newline_anycrlf -}
        , exec_newline_cr
        , exec_newline_crlf
        , exec_newline_lf
        , exec_notbol
        , exec_noteol
        , exec_notempty
        , exec_no_utf8_check
        , exec_partial

    ) where

import qualified Data.ByteString.Char8 as S
import qualified Text.Regex.PCRE.Light as S
import Text.Regex.PCRE.Light hiding (match, compile, compileM)

-- | 'compile'
--
-- Compile a perl-compatible regular expression, in a strict bytestring.
-- The arguments are:
--
-- * 'pat': A ByteString, which may or may not be zero-terminated,
-- containing the regular expression to be compiled. 
--
-- * 'flags', optional bit flags. If 'Nothing' is provided, defaults are used.
--
-- Valid compile-time flags are:
--
-- * 'anchored'        - Force pattern anchoring
--
-- * 'auto_callout'    - Compile automatic callouts
--
-- * 'bsr_anycrlf'     - \\R matches only CR, LF, or CRLF
--
-- * 'bsr_unicode'     - \\R matches all Unicode line endings
--
-- * 'caseless'        - Do caseless matching
--
-- * 'dollar_endonly'  - '$' not to match newline at end
--
-- * 'dotall'          - matches anything including NL
--
-- * 'dupnames'        - Allow duplicate names for subpatterns
--
-- * 'extended'        - Ignore whitespace and # comments
--
-- * 'extra'           - PCRE extra features (not much use currently)
--
-- * 'firstline'       - Force matching to be  before  newline
--
-- * 'multiline'       - '^' and '$' match newlines within data
--
-- * 'newline_any'     - Recognize any Unicode newline sequence
--
-- * 'newline_anycrlf' - Recognize CR, LF, and CRLF as newline sequences
--
-- * 'newline_cr'      - Set CR as the newline sequence
--
-- * 'newline_crlf'    - Set CRLF as the newline sequence
--
-- * 'newline_lf'      - Set LF as the newline sequence
--
-- * 'no_auto_capture' - Disable numbered capturing parentheses (named ones available)
--
-- * 'ungreedy'        - Invert greediness of quantifiers
--
-- * 'utf8'            - Run in UTF-8 mode
--
-- * 'no_utf8_check'   - Do not check the pattern for UTF-8 validity
--
-- If compilation of the pattern fails, the 'Left' constructor is 
-- returned with the error string. Otherwise an abstract type
-- representing the compiled regular expression is returned.
-- The regex is allocated via malloc on the C side, and will be
-- deallocated by the runtime when the Haskell value representing it
-- goes out of scope.
--
-- As regexes are often defined statically, GHC will compile them 
-- to null-terminated, strict C strings, enabling compilation of the 
-- pattern without copying. This may be useful for very large patterns.
--
-- See man pcreapi for more details.
--
compile :: String -> [PCREOption] -> Regex
compile str os = S.compile (S.pack str) os
{-# INLINE compile #-}

-- | 'compileM'
-- A safe version of 'compile' with failure lifted into an Either
compileM :: String -> [PCREOption] -> Either String Regex
compileM str os = S.compileM (S.pack str) os
{-# INLINE compileM #-}


-- | 'match'
--
-- Matches a compiled regular expression against a given subject string,
-- using a matching algorithm that is similar to Perl's. If the subject
-- string doesn't match the regular expression, 'Nothing' is returned,
-- otherwise the portion of the string that matched is returned, along
-- with any captured subpatterns.
--
-- The arguments are:
--
-- * 'regex', a PCRE regular expression value produced by compile
--
-- * 'subject', the subject string to match against
--
-- * 'options', an optional set of exec-time flags to exec.
--
-- Available runtime options are:
--
-- * 'anchored'        - Match only at the first position
--
-- * 'bsr_anycrlf'     - '\\R' matches only CR, LF, or CRLF
--
-- * 'bsr_unicode'     - '\\R' matches all Unicode line endings
--
-- * 'newline_any'     - Recognize any Unicode newline sequence
--
-- * 'newline_anycrlf' - Recognize CR, LF, and CRLF as newline sequences
--
-- * 'newline_cr'      - Set CR as the newline sequence
--
-- * 'newline_crlf'    - Set CRLF as the newline sequence
--
-- * 'newline_lf'      - Set LF as the newline sequence
--
-- * 'notbol'          - Subject is not the beginning of a line
--
-- * 'noteol'          - Subject is not the end of a line
--
-- * 'notempty'        - An empty string is not a valid match
--
-- * 'no_utf8_check'   - Do not check the subject for UTF-8
--
-- * 'partial'         - Return PCRE_ERROR_PARTIAL for a partial match
--
-- The result value, and any captured subpatterns, are returned.
-- If the regex is invalid, or the subject string is empty, Nothing
-- is returned.
--
match :: Regex -> String -> [PCREExecOption] -> Maybe [String]
match r subject os =
    case S.match r (S.pack subject) os of
           Nothing -> Nothing
           Just x  -> Just (map S.unpack x)
{-# INLINE match #-}
