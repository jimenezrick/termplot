{-# LANGUAGE ForeignFunctionInterface #-}

module Sysconf (
    Sysconf(..)
  , sysconf
) where

import Foreign.C

#include <unistd.h>

data Sysconf = ClkTck
             | NprocessorsOnln

getConst :: Sysconf -> CInt
getConst x = case x of
               ClkTck          -> #const _SC_CLK_TCK
               NprocessorsOnln -> #const _SC_NPROCESSORS_ONLN

foreign import ccall unsafe "unistd.h sysconf" c_sysconf :: CInt -> IO CLong

sysconf :: Sysconf -> IO Integer
sysconf x = do
    resetErrno
    r <- c_sysconf $ getConst x
    case fromIntegral r of
      -1 -> do
          errno <- getErrno
          if errno == eINVAL
            then throwErrno "sysconf"
            else return (-1)
      r' -> return r'
