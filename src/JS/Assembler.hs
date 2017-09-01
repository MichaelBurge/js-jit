{-# LANGUAGE ForeignFunctionInterface #-}

module JS.Assembler where

import Protolude

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Marshal.Utils (copyBytes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB

data Instruction = IRet

assemble :: [Instruction] -> BS.ByteString
assemble is = BSL.toStrict $ BSB.toLazyByteString $ mconcat $ map assembleOne is
  where
    assembleOne :: Instruction -> BSB.Builder
    assembleOne = \case
      IRet -> b 0xc3
    b = BSB.word8

load :: BS.ByteString -> IO (FunPtr (IO ()))
load bs = do
  let size = fromIntegral $ BS.length bs
      flags = prot_write .|. prot_exec
  dest <- c_valloc size
  prot <- c_mprotect dest size flags
  if prot == 0
    then return ()
    else throwErrno "load:mprotect"
  BS.unsafeUseAsCString bs $ \src ->
    copyBytes dest (castPtr src) (BS.length bs)
  return $ castPtrToFunPtr dest

prot_write = 0x2
prot_exec  = 0x4
  
exec :: FunPtr (IO ()) -> IO ()
exec fun = mkFun fun
  
         
type CFlags = CInt

foreign import ccall unsafe "malloc.h valloc"
  c_valloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "sys/mman.h mprotect"
  c_mprotect :: Ptr () -> CSize -> CFlags -> IO CInt
foreign import ccall unsafe "dynamic"
  mkFun :: FunPtr (IO ()) -> IO ()
