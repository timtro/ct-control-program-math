{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Data.Fix  (Fix, foldFix)
import           BiCCC
import           PrAlgebra
import           MCoalgebra
import           Test.HUnit

(▽) :: (() -> t) -> ((a, b) -> t) -> 𝘗ᵣ b a -> t
(ge ▽ f) x = case x of
  (Pᵣ Nothing)         -> ge()
  (Pᵣ (Just (tl, hd))) -> f (tl, hd)

type S = Int
type I = Int
type O = Int
type Y = Int
type R = Int


gSZero :: GlobalElement S
gSZero = makeGlobal 0

algSum :: 𝘗ᵣ I S -> S
algSum = gSZero ▽ f
  where
    f :: (S, I) -> S
    f (s, i) = s + i

gSZeroZero :: GlobalElement (Y, R)
gSZeroZero = makeGlobal (0,0)

combineLatestAlg :: 𝘗ᵣ (Either Y R) (Y, R) -> (Y, R)
combineLatestAlg = gSZeroZero ▽ ψ
  where
    ψ :: ((y, r), Either y r) → (y, r)
    ψ ((y, r), Left y') = (y', r)
    ψ ((y, r), Right r') = (y, r')

testSum = TestCase(assertEqual "Blah." expected calculated)
  where
    expected = [0, 0, 1, 3, 6, 10]
    calculated = snocToList . scanFix algSum $ is
    is :: Snoc I
    is = nil ⧺ 0 ⧺ 1 ⧺ 2 ⧺ 3 ⧺ 4


testCombineLatestAlg = TestCase(assertEqual "Foobar." expected calculated)
  where 
    calculated = snocToList $ scanFix combineLatestAlg yORrs
    expected = [(0,0), (1,0), (2,0), (3, 0), (3,1)]
    yORrs :: Snoc (Either Y R)
    yORrs = nil ⧺ Left 1 ⧺ Left 2 ⧺ Left 3 ⧺ Right 1

tests = TestList [
  TestLabel "Test scanFix/scanify using simple running sum algebra" testSum,
  TestLabel "Test combineLatestAlg" testCombineLatestAlg]

main :: IO Counts
main = do
  runTestTT tests

-- main = interact $ snocString . scanFix algSum . listToSnoc .  map read . words
