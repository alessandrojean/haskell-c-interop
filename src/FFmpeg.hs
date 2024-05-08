{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module FFmpeg where

import Control.Monad (forM_, join)
import Data.Maybe
import Foreign
import Foreign.C.Types
import Foreign.C (CString, withCString, peekCString)
import Foreign.CStorable (CStorable(..))
import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as Map

import PrettyPrint

data FfmpegInput = FfmpegInput
  { ffMetadata :: AvDictionary
  , ffFormat :: AvDictionary
  , ffStreamsMetadata :: Ptr AvDictionary
  , ffStreamsNb :: CInt
  , ffContext :: Ptr ()
  } deriving (Generic, CStorable)

-- instance Storable FfmpegInput where
--   alignment _ = 8
--   sizeOf _ = 36
--   peek ptr = FfmpegInput
--     <$> peekByteOff ptr 0
--     <*> peekByteOff ptr 8
--     <*> peekByteOff ptr 16
--     <*> peekByteOff ptr 24
--     <*> peekByteOff ptr 32
--   poke _ _ = undefined

instance Storable FfmpegInput where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

type AvDictionary = Ptr ()

data AvDictionaryEntry = AvDictionaryEntry
  { avDictKey :: CString
  , avDictValue :: CString
  } deriving (Generic, CStorable)

-- instance Storable AvDictionaryEntry where
--   alignment _ = 8
--   sizeOf _ = 16
--   peek ptr = AvDictionaryEntry 
--     <$> peekByteOff ptr 0
--     <*> peekByteOff ptr 8
--   poke ptr (AvDictionaryEntry k v) = do
--     pokeByteOff ptr 0 k
--     pokeByteOff ptr 8 v

instance Storable AvDictionaryEntry where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

foreign import capi "ffmpeg.h load_input"
  c_loadInput :: CString -> IO (Ptr FfmpegInput)

foreign import capi "ffmpeg.h &free_input"
  c_freeInput :: FunPtr (Ptr FfmpegInput -> IO ())

foreign import capi "libavutil/dict.h av_dict_get"
  c_avDictGet :: AvDictionary -> CString -> Ptr AvDictionaryEntry -> CInt -> IO (Ptr AvDictionaryEntry)

foreign import capi "libavutil/dict.h value AV_DICT_IGNORE_SUFFIX"
  c_avDictIgnoreSuffix :: CInt

loadInput :: String -> IO (Maybe (ForeignPtr FfmpegInput))
loadInput file = do
  inputPtr <- withCString file $ \c_file -> c_loadInput c_file
  foreignPtr <- newForeignPtr c_freeInput inputPtr
  return $ if inputPtr == nullPtr then Nothing else Just foreignPtr

dictToMap :: AvDictionary -> IO (Map String String)
dictToMap dict = do
  firstTag <- dictGet dict nullPtr
  dictToMap' dict Map.empty firstTag

dictToMap' :: AvDictionary -> Map String String -> Ptr AvDictionaryEntry -> IO (Map String String)
dictToMap' dict mp tagPtr | tagPtr == nullPtr = pure mp
                          | otherwise         = nextCall
  where
    key = peek tagPtr >>= \t -> peekCString $ avDictKey t
    value = peek tagPtr >>= \t -> peekCString $ avDictValue t
    newMap = liftA2 (\k v -> Map.insert k v mp) key value
    nextTag = dictGet dict tagPtr
    nextCall = join $ liftA2 (dictToMap' dict) newMap nextTag

dictGet :: AvDictionary -> Ptr AvDictionaryEntry -> IO (Ptr AvDictionaryEntry)
dictGet dict previous = withCString "" $ \c_str ->
  c_avDictGet dict c_str previous c_avDictIgnoreSuffix

printMetadata :: String -> IO ()
printMetadata fileName = do
  inputPtr' <- loadInput fileName
  let inputForeignPtr = fromJust inputPtr'

  withForeignPtr inputForeignPtr $ \inputPtr -> do
    input <- peek inputPtr
    metadata <- dictToMap $ ffMetadata input
    formatMetadata <- dictToMap $ ffFormat input

    let nbStreams = fromIntegral . toInteger $ ffStreamsNb input
        streamsMetadataPtr = ffStreamsMetadata input

    streamsMetadata <- peekArray nbStreams streamsMetadataPtr

    prettyTitle "File metadata"
    prettyPrint metadata
    putStrLn ""

    prettyTitle "Format metadata"
    prettyPrint formatMetadata
    putStrLn ""

    forM_ (zip [0..nbStreams-1] streamsMetadata) $ \(i, sMetadataPtr) -> do
      sMetadata <- dictToMap sMetadataPtr

      prettyTitle $ "Stream #" ++ show i
      prettyPrint sMetadata
      putStrLn ""
