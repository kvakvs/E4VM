module TransformAsm where

import AsmMod
import BytecodeMod

transform :: AsmMod.Module -> BytecodeMod.Module
transform m =
  BytecodeMod.new
