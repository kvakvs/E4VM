{-# LANGUAGE UnicodeSyntax #-}
module UAssembly where

data UAsmOp
  = ULabel Integer
  | ULine Integer deriving Show
