module Main where

import           Data.Fix  (Fix, foldFix)
import           PrAlgebra

type S = Int
type I = Int
type O = Int

is :: Snoc I
is = nil ⧺ 0 ⧺ 1 ⧺ 2 ⧺ 3 ⧺ 4

gSZero :: GlobalElement S
gSZero = makeGlobal 0

algSum :: 𝘗ᵣ I S -> S
algSum = gSZero ▽ f
  where
    f :: (S, I) -> S
    f (s, i) = s + i

main :: IO ()
main = do
  -- print $ snocHead . scanFix algSum . listToSnoc $ [0..100000000]
  print $ snocToList $ scanFix algSum is
-- main = interact $ snocString . scanFix algSum . listToSnoc .  map read . words
