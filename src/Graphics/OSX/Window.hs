{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Graphics.OSX.Window
-- Copyright : (c) Vincent Rasneur
-- License   : GPL2
--
-- Maintainer: Vincent Rasneur <vrasneur@free.fr>
-- Stability : provisional
-- Portability: unportable
--
--------------------------------------------------------------------
--
-- Interface to the windows and idle time functions in Mac OS X
--

module Graphics.OSX.Window
       ( fetchWindowTitles
       , getForegroundWindow
       , getIdleTime
       ) where

import Control.Monad

import Data.Maybe (fromJust, fromMaybe, isNothing, isJust, catMaybes)
import Data.Bits (shiftL, (.|.))

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, mallocBytes, free)
import Foreign.Storable (peek)

import System.IO (hPutStrLn, stderr)

-- Core Foundation basic types

type Boolean = CUChar
bTRUE = 1 :: Boolean
bFALSE = 0 :: Boolean

type CFTypeID = CULong

type CFIndex = CLong

type CFStringEncoding = CUInt

-- Core Foundation pointer types

data CFType = CFType
type CFTypeRef = Ptr CFType

data CFNumber = CFNumber
type CFNumberRef = Ptr CFNumber

data CFString = CFString
type CFStringRef = Ptr CFString

data CFArray = CFArray
type CFArrayRef = Ptr CFArray

data CFDictionary = CFDictionary
type CFDictionaryRef = Ptr CFDictionary
type CFMutableDictionaryRef = Ptr CFDictionary

data CFAllocator = CFAllocator
type CFAllocatorRef = Ptr CFAllocator

-- IOKit types

type Ckern_return_t = CInt
iKERN_SUCCESS = 0 :: Ckern_return_t

type Cmach_port_t = CUInt
type Cio_object_t = Cmach_port_t
type Cio_iterator_t = Cio_object_t
type Cio_registry_entry_t = Cio_object_t
type CIOOptionBits = CUInt

-- type getter functions

foreign import ccall "CFGetTypeID" c_CFGetTypeID :: CFTypeRef -> IO CFTypeID

foreign import ccall "CFNumberGetTypeID" c_CFNumberGetTypeID :: IO CFTypeID
foreign import ccall "CFStringGetTypeID" c_CFStringGetTypeID :: IO CFTypeID
foreign import ccall "CFArrayGetTypeID" c_CFArrayGetTypeID :: IO CFTypeID
foreign import ccall "CFDictionaryGetTypeID" c_CFDictionaryGetTypeID :: IO CFTypeID

-- memory management functions

foreign import ccall "CFRetain" c_CFRetain :: CFTypeRef -> IO ()
foreign import ccall "CFRelease" c_CFRelease :: CFTypeRef -> IO ()

-- allocator variables

foreign import ccall "&kCFAllocatorDefault" c_kCFAllocatorDefaultPtr :: Ptr CFAllocatorRef
foreign import ccall "&kCFAllocatorNull" c_kCFAllocatorNullPtr :: Ptr CFAllocatorRef

c_kCFAllocatorDefault :: IO CFAllocatorRef
c_kCFAllocatorDefault = peek c_kCFAllocatorDefaultPtr

c_kCFAllocatorNull :: IO CFAllocatorRef
c_kCFAllocatorNull = peek c_kCFAllocatorNullPtr

-- number functions

kCFNumberSInt64Type = 4 :: CInt

foreign import ccall unsafe "CFNumberGetValue" c_CFNumberGetValue :: CFNumberRef -> CInt -> Ptr a -> IO Boolean

-- string functions

kCFStringEncodingUTF8 = 0x08000100 :: CFStringEncoding

foreign import ccall unsafe "CFStringGetLength" c_CFStringGetLength :: CFStringRef -> IO CFIndex
foreign import ccall unsafe "CFStringGetMaximumSizeForEncoding" c_CFStringGetMaximumSizeForEncoding :: CFIndex -> CFStringEncoding -> IO CFIndex
foreign import ccall unsafe "CFStringGetCStringPtr" c_CFStringGetCStringPtr :: CFStringRef -> CFStringEncoding -> IO CString
foreign import ccall unsafe "CFStringGetCString" c_CFStringGetCString :: CFStringRef -> CString -> CFIndex -> CFStringEncoding -> IO Boolean
foreign import ccall unsafe "CFStringCreateWithCStringNoCopy" c_CFStringCreateWithCStringNoCopy :: CFAllocatorRef -> CString -> CFStringEncoding -> CFAllocatorRef -> IO CFStringRef

-- array functions

foreign import ccall unsafe "CFArrayGetCount" c_CFArrayGetCount :: CFArrayRef -> IO CLong
foreign import ccall unsafe "CFArrayGetValueAtIndex" c_CFArrayGetValueAtIndex :: CFArrayRef -> CFIndex -> IO CFTypeRef

-- dictionary functions

foreign import ccall unsafe "CFDictionaryGetValue" c_CFDictionaryGetValue :: CFDictionaryRef -> Ptr a -> IO (Ptr b)

-- window functions

type CGWindowListOption = CUInt
kCGWindowListOptionOnScreenOnly = (1 `shiftL` 0) :: CGWindowListOption
kCGWindowListExcludeDesktopElements = (1 `shiftL` 4) :: CGWindowListOption

type CGWindowID = CUInt
kCGNullWindowID = 0 :: CGWindowID

foreign import ccall unsafe "CGWindowListCopyWindowInfo" c_CGWindowListCopyWindowInfo :: CGWindowListOption -> CGWindowID -> IO CFArrayRef

-- IOKit functions

cMACH_PORT_NULL = 0 :: Cmach_port_t

foreign import ccall unsafe "IOObjectRelease" c_IOObjectRelease :: Cio_object_t -> IO Ckern_return_t
foreign import ccall unsafe "IOMasterPort" c_IOMasterPort :: Cmach_port_t -> Ptr Cmach_port_t -> IO Ckern_return_t
foreign import ccall unsafe "IOServiceGetMatchingServices" c_IOServiceGetMatchingServices :: Cmach_port_t -> CFDictionaryRef -> Ptr Cio_iterator_t -> IO Ckern_return_t
foreign import ccall unsafe "IOServiceMatching" c_IOServiceMatching :: CString -> IO CFMutableDictionaryRef 
foreign import ccall unsafe "IOIteratorNext" c_IOIteratorNext :: Cio_iterator_t -> IO Cio_object_t
foreign import ccall unsafe "IORegistryEntryCreateCFProperty" c_IORegistryEntryCreateCFProperty :: Cio_registry_entry_t -> CFStringRef -> CFAllocatorRef -> CIOOptionBits -> IO CFTypeRef

-- misc utilities

cond :: Monad m => Bool -> a -> m a -> m a
cond True  val  _ = return val
cond False _    a = a

condM :: Monad m => Bool -> m a -> m a -> m a
condM True  val _ = val
condM False _   a = a

condMsg :: Bool -> String -> a -> IO a -> IO a
condMsg True  msg val _ = hPutStrLn stderr msg >> return val
condMsg False _   _   a = a

-- type utilities

isTypeRef :: IO CULong -> CFTypeRef -> IO Bool
isTypeRef typeFun ref = do
         cond (ref == nullPtr) False $ do
           typ <- c_CFGetTypeID ref
           funTyp <- typeFun
           return (typ == funTyp)

isStringRef :: CFTypeRef -> IO Bool
isStringRef = isTypeRef c_CFStringGetTypeID

isNumberRef :: CFTypeRef -> IO Bool
isNumberRef = isTypeRef c_CFNumberGetTypeID

isArrayRef :: CFTypeRef -> IO Bool
isArrayRef = isTypeRef c_CFArrayGetTypeID

isDictionaryRef :: CFTypeRef -> IO Bool
isDictionaryRef = isTypeRef c_CFDictionaryGetTypeID

-- string utilities

getConstUTF8String :: CFStringRef -> IO CString
getConstUTF8String strRef = c_CFStringGetCStringPtr strRef kCFStringEncodingUTF8

createUTF8StringRefNoCopy :: CString -> IO CFStringRef
createUTF8StringRefNoCopy str = do
                allocDefault <- c_kCFAllocatorDefault
                allocNull <- c_kCFAllocatorNull
                c_CFStringCreateWithCStringNoCopy allocDefault str kCFStringEncodingUTF8 allocNull

-- returns malloc'ed memory (or a NULL pointer)
getUTF8String :: CFStringRef -> IO CString
getUTF8String strRef = do
              length <- c_CFStringGetLength strRef
              maxSize <- c_CFStringGetMaximumSizeForEncoding length kCFStringEncodingUTF8
              buffer <- mallocBytes $ (fromIntegral maxSize) + 1
              res <- c_CFStringGetCString strRef buffer maxSize kCFStringEncodingUTF8
              cond (res == bTRUE) buffer $ do
                free buffer
                return nullPtr

convertString :: CString -> IO (Maybe String)
convertString str = do
         cond (str == nullPtr)  Nothing $ do
           peekCString str >>= return . Just

-- Core Foundation objects conversion

getString :: CFTypeRef -> IO (Maybe String)
getString ref = isStringRef ref >>= onlyStringRef
              where
              onlyStringRef False = return Nothing
              onlyStringRef True = do
                let strRef = castPtr ref
                cstr <- getConstUTF8String strRef
                condM (cstr /= nullPtr) (convertString cstr) $ do
                   cstr <- getUTF8String strRef
                   cond (cstr == nullPtr) Nothing $ do
                     hstr <- convertString cstr
                     free cstr
                     return hstr

getInt :: CFTypeRef -> IO (Maybe Int)
getInt ref = isNumberRef ref >>= onlyNumberRef
           where
           onlyNumberRef False = return Nothing
           onlyNumberRef True = do
             let numRef = castPtr ref
             alloca $ \ptr -> do
               res <- c_CFNumberGetValue numRef kCFNumberSInt64Type (ptr :: Ptr CLong)
               condMsg (res == bFALSE)
                 "Cannot convert Core Foundation number to signed 64-bit integer." Nothing $ do
                 num <- peek ptr
                 return $ Just $ fromIntegral num

-- dictionary utilities

getDictFromArray :: CFArrayRef -> CFIndex -> IO (Maybe CFDictionaryRef)
getDictFromArray arrayRef idx = do
             ref <- c_CFArrayGetValueAtIndex arrayRef idx
             isDictionaryRef ref >>= return . onlyDictRef ref
             where
             onlyDictRef ref True = Just $ castPtr ref
             onlyDictRef ref False = Nothing

getDictValue :: CFDictionaryRef -> String -> IO CFTypeRef
getDictValue dictRef str = do
              strRef <- createUTF8StringRefNoCopy =<< newCString str
              condMsg (strRef == nullPtr)
                ("Cannot convert string '" ++ str ++"' to Core Foundation string.") nullPtr $ do
                obj <- c_CFDictionaryGetValue dictRef strRef
                c_CFRelease $ castPtr strRef
                return obj

-- IOKit utilities

getHIDSystemIterator :: IO Cio_iterator_t
getHIDSystemIterator = do
           masterPort <- alloca $ \ptr -> do
             res <- c_IOMasterPort cMACH_PORT_NULL ptr
             condMsg (res /= iKERN_SUCCESS)
               ("Cannot create master port: error " ++ show res ++ ".") 0 $
               peek ptr
           condMsg (masterPort == 0)
             "Got empty master port." 0 $
             alloca $ \ptr -> do
               dictRef <- c_IOServiceMatching =<< newCString "IOHIDSystem"
               condMsg (dictRef == nullPtr)
                 "Cannot create the IOHIDSystem matching dictionary." 0 $ do
                 res <- c_IOServiceGetMatchingServices masterPort dictRef ptr
                 condMsg (res /= iKERN_SUCCESS)
                   ("Cannot get the iterator handle: error " ++ show res ++ ".") 0 $
                   peek ptr

getHIDSystemIdleTime :: Cio_registry_entry_t -> IO CFTypeRef
getHIDSystemIdleTime entry = do
             cond (entry == 0) nullPtr $ do
               strRef <- createUTF8StringRefNoCopy =<< newCString "HIDIdleTime"
               condMsg (strRef == nullPtr)
                 "Cannot create HIDIdleTime Core Foundation string." nullPtr $ do
                 allocDefault <- c_kCFAllocatorDefault
                 ref <- c_IORegistryEntryCreateCFProperty entry strRef allocDefault 0
                 c_CFRelease $ castPtr strRef
                 condMsg (ref == nullPtr)
                   "Cannot create the HIDIdleTime property string." nullPtr $
                   return ref

withIOObject :: Cio_object_t -> IO Int -> IO Int
withIOObject obj comp = do
             condMsg (obj == 0)
               "Got empty IO object." (-1) $ do
               res <- comp
               c_IOObjectRelease obj
               return res

-- in nanoseconds
getIdleTimeNs :: IO Int
getIdleTimeNs = do
            iter <- getHIDSystemIterator
            withIOObject iter $ do
              curObj <- c_IOIteratorNext iter
              withIOObject curObj $ do
                idleRef <- getHIDSystemIdleTime curObj
                getInt idleRef >>= return . fromMaybe (-1)

-- window properties

getWindowInfo :: IO CFArrayRef
getWindowInfo = do
              let opts = kCGWindowListExcludeDesktopElements .|. kCGWindowListOptionOnScreenOnly
              c_CGWindowListCopyWindowInfo opts kCGNullWindowID
              
getWindowTitle :: CFArrayRef -> CLong -> IO (Maybe (Int, String, String))
getWindowTitle info idx = do
             dict <- getDictFromArray info idx >>= return . fromMaybe nullPtr
             condMsg (dict == nullPtr)
               ("Cannot retrieve the properties dictionary for window " ++ show idx ++ ".") Nothing $ do
               layer <- getDictValue dict "kCGWindowLayer" >>= getInt
               cond (isNothing layer || fromJust layer /= 0) Nothing $ do
                 window <- getDictValue dict "kCGWindowName" >>= getString
                 owner <- getDictValue dict "kCGWindowOwnerName" >>= getString
                 cond (isNothing window || isNothing owner) Nothing $ do
                   return $ Just (fromIntegral idx, fromJust window, fromJust owner)

fetchWindowTitles :: IO [(Int, String, String)]
fetchWindowTitles = do
                  windowInfo <- getWindowInfo
                  condMsg (windowInfo == nullPtr)
                    "Cannot get the windows information array." [] $ do
                    count <- c_CFArrayGetCount windowInfo
                    titles <- (forM [0..count - 1] $ getWindowTitle windowInfo) >>= return . catMaybes
                    c_CFRelease $ castPtr windowInfo
                    return titles

getWindowIdx :: Maybe (Int, String, String) -> Maybe Int
getWindowIdx Nothing = Nothing
getWindowIdx (Just (idleTime, _, _)) = Just idleTime

getForegroundWindowIdx :: CFArrayRef -> CLong -> CLong -> IO (Maybe Int)
getForegroundWindowIdx info idx count = do
                  cond (count == 0) Nothing $ do
                    title <- getWindowTitle info idx
                    cond (isJust title) (getWindowIdx title) $
                      getForegroundWindowIdx info (idx + 1) (count - 1)

getForegroundWindow :: IO Int
getForegroundWindow = do
                    windowInfo <- getWindowInfo
                    condMsg (windowInfo == nullPtr)
                      "Cannot get the windows information array." (-1) $ do
                      count <- c_CFArrayGetCount windowInfo
                      idx <- getForegroundWindowIdx windowInfo 0 count
                      c_CFRelease $ castPtr windowInfo
                      return $ fromMaybe (-1) idx

-- idle time

-- in milliseconds
getIdleTime :: IO Integer
getIdleTime = getIdleTimeNs >>= return . \idleTime -> quot (fromIntegral idleTime) 1000000
