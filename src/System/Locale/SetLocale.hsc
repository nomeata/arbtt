{-# LANGUAGE ForeignFunctionInterface #-}

{-
This file is copied from the setlocale-0.0.3 package. Its author is Lukas Mai
and it is placed in the Public Domain.
-}

module System.Locale.SetLocale (
    Category(..),
    categoryToCInt,
    setLocale
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Data.Typeable

-- | A type representing the various locale categories. See @man 7 locale@.
data Category
    = LC_ALL
    | LC_COLLATE
    | LC_CTYPE
    | LC_MESSAGES
    | LC_MONETARY
    | LC_NUMERIC
    | LC_TIME
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Typeable Category where
    typeOf _ = mkTyConApp (mkTyCon "System.Locale.SetLocale.Category") []

#include <locale.h>

-- | Convert a 'Category' to the corresponding system-specific @LC_*@ code.
-- You probably don't need this function.
categoryToCInt :: Category -> CInt
categoryToCInt LC_ALL = #const LC_ALL
categoryToCInt LC_COLLATE = #const LC_COLLATE
categoryToCInt LC_CTYPE = #const LC_CTYPE
categoryToCInt LC_MESSAGES = #const LC_MESSAGES
categoryToCInt LC_MONETARY = #const LC_MONETARY
categoryToCInt LC_NUMERIC = #const LC_NUMERIC
categoryToCInt LC_TIME = #const LC_TIME

ptr2str :: Ptr CChar -> IO (Maybe String)
ptr2str p
    | p == nullPtr = return Nothing
    | otherwise = fmap Just $ peekCString p

str2ptr :: Maybe String -> (Ptr CChar -> IO a) -> IO a
str2ptr Nothing  f = f nullPtr
str2ptr (Just s) f = withCString s f

foreign import ccall unsafe "locale.h setlocale" c_setlocale :: CInt -> Ptr CChar -> IO (Ptr CChar)

-- | A Haskell version of @setlocale()@. See @man 3 setlocale@.
setLocale :: Category -> Maybe String -> IO (Maybe String)
setLocale cat str =
    str2ptr str $ \p -> c_setlocale (categoryToCInt cat) p >>= ptr2str
