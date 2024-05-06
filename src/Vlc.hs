{-# LANGUAGE CApiFFI #-}
module Vlc where

import Control.Concurrent (threadDelay)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.ConstPtr

foreign import capi "vlc/vlc.h libvlc_new" c_vlcNew :: CInt -> Ptr (ConstPtr CString) -> IO (Ptr ())
foreign import capi "vlc/vlc.h libvlc_media_new_path" c_vlcMediaNewPath
  :: Ptr () -> CString -> IO (Ptr ())
foreign import capi "vlc/vlc.h libvlc_media_player_new_from_media" c_vlcMediaPlayerNewFromMedia
  :: Ptr () -> IO (Ptr ())

foreign import capi "vlc/vlc.h libvlc_media_player_play" c_vlcMediaPlayerPlay
  :: Ptr () -> IO ()
foreign import capi "vlc/vlc.h libvlc_media_player_stop" c_vlcMediaPlayerStop
  :: Ptr () -> IO ()

foreign import capi "vlc/vlc.h &libvlc_media_release" c_vlcMediaRelease
  :: FunPtr (Ptr () -> IO ())
foreign import capi "vlc/vlc.h &libvlc_media_player_release" c_vlcMediaPlayerRelease
  :: FunPtr (Ptr () -> IO ())
foreign import capi "vlc/vlc.h &libvlc_release" c_vlcRelease
  :: FunPtr (Ptr () -> IO ())

vlcNew :: CInt -> Ptr (ConstPtr CString) -> IO (ForeignPtr ())
vlcNew argc argv = c_vlcNew argc argv >>= newForeignPtr c_vlcRelease

vlcMediaNewPath :: ForeignPtr () -> String -> IO (ForeignPtr ())
vlcMediaNewPath inst file = 
  withForeignPtr inst (withCString file . c_vlcMediaNewPath) 
    >>= newForeignPtr c_vlcMediaRelease

vlcMediaPlayerNewFromMedia :: ForeignPtr () -> IO (ForeignPtr ())
vlcMediaPlayerNewFromMedia media = 
  withForeignPtr media c_vlcMediaPlayerNewFromMedia 
    >>= newForeignPtr c_vlcMediaPlayerRelease

vlcMediaPlayerPlay :: ForeignPtr () -> IO ()
vlcMediaPlayerPlay player = withForeignPtr player c_vlcMediaPlayerPlay

vlcMediaPlayerStop :: ForeignPtr () -> IO ()
vlcMediaPlayerStop player = withForeignPtr player c_vlcMediaPlayerStop

playFile :: String -> IO ()
playFile fileName = do
  inst <- vlcNew 0 nullPtr
  m <- vlcMediaNewPath inst fileName
  mp <- vlcMediaPlayerNewFromMedia m

  vlcMediaPlayerPlay mp
  threadDelay $ 10 * 1000000
  vlcMediaPlayerStop mp

  return ()
