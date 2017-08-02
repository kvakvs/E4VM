-- Implementation comes from a Github Gist by ibtaylor/Huffman.hs
module Bytecode.Encode.Huffman (encode) where

import qualified Bits        as B

import           Data.Either
import qualified Data.List   as L
import qualified Data.Map    as M
import           Data.Maybe
import qualified Data.Ord    as DO

-- val contains either leaf value or tree children. Bits is bit length
data HuffmanTree
  = LeafNode Int
             Int
  | InternalNode Int
                 HuffmanTree
                 HuffmanTree
  deriving (Eq)

instance Show HuffmanTree where
  show = go ""
    where
      spaces = map (const ' ')
      paren s = "(" ++ s ++ ")"
      go ss (LeafNode s o) = "--" ++ paren (show o) ++ show s ++ "\n"
      go ss (InternalNode o l r) =
        let root = "--" ++ paren (show o) ++ "-+"
            ss' = ss ++ tail (spaces root)
            lbranch = go (ss' ++ "|") l
            rbranch = go (ss' ++ " ") r
        in root ++ lbranch ++ ss' ++ "|\n" ++ ss' ++ "`" ++ rbranch

frequency :: HuffmanTree -> Int
frequency (LeafNode x _)       = x
frequency (InternalNode x _ _) = x

type HDict = M.Map Int B.Bits

data Encoded = Encoded
  { eCode :: B.BitsList
  , eTree :: HuffmanTree
  , eDict :: HDict
  }

type Frequency = (Int, Int)

encode :: [Int] -> HuffmanTree
encode input = makeTree $ makeFreqTable (L.sort input) []

-- Group list contents by value, and collect counts for each value in 'accum'
makeFreqTable :: [Int] -> [Frequency] -> [Frequency]
makeFreqTable [] accum = accum
makeFreqTable (h:tl) accum =
  let (block, moreBlocks) = L.partition (h ==) tl
      newItem = (h, 1 + length block)
  in makeFreqTable moreBlocks (newItem : accum)

sortHelperFn (_v1, count1) (_v2, count2)
  | count1 < count2 = LT
  | count1 > count2 = GT
sortHelperFn _ _ = EQ

makeTree :: [Frequency] -> HuffmanTree
makeTree
  -- First convert each freq tuple into a leaf then combine
 = combine . map toLeaf
        -- Repeatedly combine lowest freq trees and reinsert the result into
        -- the freq ordered list (todo: priority queue)
  where
    combine [t] = t
    combine (ta:tb:ts) =
      combine . L.insertBy (DO.comparing frequency) (merge ta tb) $ ts
        -- make an internal node from 2 trees. The freq is sum of two trees freq
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
        -- make a leaf from (symbol,freq)
    toLeaf = uncurry LeafNode
