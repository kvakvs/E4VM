{-# LANGUAGE ExplicitForAll #-}

-- Implementation comes from a Github Gist by ibtaylor/Huffman.hs
-- https://gist.github.com/ibtaylor/1024266
--
module Bitcode.Encode.Huffman
  ( makeEncoderFromImport
  , makeEncoderFromFreq
  , encodeSome
  , makeFreqTable
  , Encoder
  , Frequency(..)
  ) where

import qualified Bits       as B

import qualified Data.List  as L
import qualified Data.Map   as M
import           Data.Maybe
import           Data.Ord

-- val contains either leaf value or tree children. Bits is bit length
data HuffmanTree a
  = LeafNode a
             Int
  | InternalNode Int
                 (HuffmanTree a)
                 (HuffmanTree a)
  deriving (Eq)

instance Show a => Show (HuffmanTree a) where
  show = go ""
    where
      spaces = map (const ' ')
      go _accum (LeafNode val nBits) =
        "--b:" ++ show nBits ++ ",val:" ++ show val ++ "\n"
      go accum (InternalNode nBits left right) =
        let root = "--b:" ++ show nBits ++ "-+"
            ss' = accum ++ tail (spaces root)
            lbranch = go (ss' ++ "|") left
            rbranch = go (ss' ++ " ") right
        in root ++ lbranch ++ ss' ++ "|\n" ++ ss' ++ "`" ++ rbranch

frequency :: HuffmanTree a -> Int
frequency (LeafNode _a x)        = x
frequency (InternalNode x _l _r) = x

--type HDict = M.Map Int B.Bits
data Encoder a = Encoder
  { eCodes :: M.Map a [Bool]
  , eTree :: HuffmanTree a
  } deriving (Show)

data Frequency a =
  Frequency a
            Int
  deriving (Eq)

instance Show a => Show (Frequency a) where
  show (Frequency val freq) = show val ++ " (x" ++ show freq ++ ")"

instance Eq a => Ord (Frequency a) where
  compare (Frequency _val1 freq1) (Frequency _val2 freq2) = compare freq1 freq2

--makeFrequency :: a -> Int -> (a, Int)
--makeFrequency val freq = Frequency val freq
makeEncoderFromImport ::
     Ord a
  => Eq a =>
       [a] -> Encoder a
makeEncoderFromImport input = Encoder {eTree = tree, eCodes = codes}
  where
    tree = makeTree $ makeFreqTable input
    codes = makeCodes tree

makeEncoderFromFreq :: Ord a => [Frequency a] -> Encoder a
makeEncoderFromFreq freq = Encoder {eTree = tree, eCodes = codes}
  where
    tree = makeTree freq
    codes = makeCodes tree

-- traverse the huffman tree generating a map from the symbol to its huffman
-- tree path (where False is left, and True is right)
makeCodes :: Ord a => HuffmanTree a -> M.Map a [Bool]
makeCodes = M.fromList . go []
    -- leaf nodes mark the end of a path to a symbol
  where
    go p (LeafNode s _)       = [(s, reverse p)]
    -- traverse both branches and accumulate a reverse path
    go p (InternalNode _ l r) = go (True : p) l ++ go (False : p) r

-- from a table mapping symbols to their corresponding huffman tree bit paths,
-- replace each instance of a symbol with its bit path
encodeSome :: Ord a => Encoder a -> [a] -> B.Bits
encodeSome Encoder {eCodes = tbl} = B.makeBits . concatMap get
  where
    get x = fromJust (M.lookup x tbl)

makeFreqTable ::
     Ord a
  => Eq a =>
       [a] -> [Frequency a]
makeFreqTable input = makeFreqTable' (L.sort input) []

-- Group list contents by value, and collect counts for each value in 'accum'
makeFreqTable' :: Eq a => [a] -> [Frequency a] -> [Frequency a]
makeFreqTable' [] accum = accum
makeFreqTable' (h:tl) accum =
  let (block, moreBlocks) = L.partition (h ==) tl
      newItem = Frequency h (1 + length block)
  in makeFreqTable' moreBlocks (newItem : accum)

makeTree :: [Frequency a] -> HuffmanTree a
makeTree inp
  -- First convert each freq tuple into a leaf then combine
 = combine inp2
        -- Repeatedly combine lowest freq trees and reinsert the result into
        -- the freq ordered list (todo: priority queue)
  where
    inp2 = map toLeaf inp
    combine [t] = t
    combine (ta:tb:ts) =
      combine . L.insertBy (comparing frequency) (merge ta tb) $ ts
        -- make an internal node from 2 trees. The freq is sum of two trees freq
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
        -- make a leaf from (symbol,freq)
    toLeaf (Frequency val freq) = LeafNode val freq
