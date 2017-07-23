module Bytecode where

import qualified Data.ByteString as B

data BcOp = BcOp Int [B.ByteString] deriving Show

