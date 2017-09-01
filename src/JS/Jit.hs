module JS.Jit where

import Protolude

import JS.Ast

import qualified Data.ByteString as BS

data Pad = Pad {
  
               } deriving (Eq, Show)

data Assembly = Assembly [Instruction]

data Instruction = IAdd deriving (Eq, Show)

compile :: Statement -> Assembly
compile = undefined

assemble :: Assembly -> BS.ByteString
assemble = undefined

exec :: BS.ByteString -> IO ()
exec = undefined
