module JS.Jit where

import Protolude

import JS.Ast

import qualified Data.ByteString as BS

data Pad = Pad {
  
               } deriving (Eq, Show)

data Assembly = Assembly BS.ByteString

data Instruction = IAdd deriving (Eq, Show)

assemble :: Statement -> Assembly
assemble = undefined
