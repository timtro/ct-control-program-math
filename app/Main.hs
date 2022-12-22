module Main where

import           Data.Fix  (Fix, foldFix)
import           PrAlgebra

type S = Int
type I = Int
type O = Int

is :: Snoc I
is = nil ⧺ 0 ⧺ 1 ⧺ 2 ⧺ 3 ⧺ 4

algSum :: 𝘗ᵣAlgebra Int Int
algSum (Pᵣ Nothing) = 0
algSum (Pᵣ (Just (s, i))) = f s i
  where
    f :: S -> I -> S
    f s i = s + i

main :: IO ()
main = do
  print $ snocToList $ scanFix algSum is
