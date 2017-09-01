{-# LANGUAGE ForeignFunctionInterface #-}

module JS.Special where

import Protolude

import Foreign.C.Types

foreign import ccall "&log" a_log :: FunPtr (CDouble -> IO ())

