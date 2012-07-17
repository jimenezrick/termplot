{-# LANGUAGE ForeignFunctionInterface #-}

module Sysconf where

import Foreign.C

foreign import ccall unsafe "unistd.h sysconf" c_sysconf :: CInt -> IO CLong

sysconf :: Int -> IO Integer
sysconf x = do
    resetErrno
    r <- c_sysconf $ fromIntegral x
    case fromIntegral r of
      -1 -> do
          errno <- getErrno
          if errno == eINVAL
            then throwErrno "sysconf"
            else return (-1)
      r' -> return r'
