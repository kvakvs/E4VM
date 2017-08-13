-- Module represents data structure and functions to create .uerl bitcode
-- files with sections and stuff
module Bitcode.UerlFile where

import qualified Bits as B

data UerlFile = UerlFile {
  header :: [B.Bits]
}
