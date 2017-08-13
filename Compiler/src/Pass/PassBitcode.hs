-- Second pass for Asm, processes bitcode instructions and creates bit stream
-- output with compressed opcodes
module Pass.PassBitcode
  ( transform
  ) where

import qualified Asm.Mod              as AM
import qualified Bitcode.Mod          as BM
import qualified Bitcode.UerlFile     as BF

import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Bitcode module produce Bitcode compressed opcodes
transform :: BM.Module -> BF.UerlFile
transform bcmod =
  let funs = BM.funs bcmod
      funs1 = S.execState (transformM funs) []
