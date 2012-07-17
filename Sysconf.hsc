{-# LANGUAGE ForeignFunctionInterface #-}

module Sysconf where

import Foreign.C

#include <unistd.h>

type Sysconf = Int

scClkTck          = #const _SC_CLK_TCK
scNprocessorsOnln = #const _SC_NPROCESSORS_ONLN

foreign import ccall unsafe "unistd.h sysconf" c_sysconf :: CInt -> IO CLong

sysconf :: Sysconf -> IO Integer
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
