{-# LANGUAGE FlexibleInstances  #-}

module PrAlgebra where

import Data.Fix ( foldFix, Fix(Fix), unFix )

(▽) :: (a -> c) -> (b -> c) -> Either a b -> c
(▽) = either

(△) :: (b -> c) -> (b -> c') -> b -> (c, c')
(△) f g x = (f x, g x)

newtype 𝘗ᵣ hd tl = Pᵣ (Maybe (tl, hd))

instance Functor (𝘗ᵣ hd) where
  fmap f (Pᵣ Nothing) = Pᵣ Nothing
  fmap f (Pᵣ (Just (tl, hd))) = Pᵣ (Just (f tl, hd))

type Snoc hd = Fix(𝘗ᵣ hd)

snoc :: Snoc a -> a -> Snoc a
snoc xs x = Fix (Pᵣ (Just (xs, x)))

(⧺) :: Snoc a -> a -> Snoc a
(⧺) = snoc

nil :: Snoc a
nil = Fix (Pᵣ Nothing)

type 𝘗ᵣAlgebra state value =  𝘗ᵣ value state -> state

snocHead :: Snoc hd -> hd
snocHead (Fix (Pᵣ Nothing)) = error "snocHead called on empty Snoc List"
snocHead (Fix (Pᵣ( Just (xs, x) ) ) ) = x

snocToList :: Snoc hd -> [hd]
snocToList = foldFix alg
  where
    -- alg :: 𝘗ᵣ hd [hd] -> [hd]
    alg :: 𝘗ᵣAlgebra [hd] hd
    alg (Pᵣ Nothing) = []
    alg (Pᵣ (Just (accum, val))) = accum ++ [val]

snocLen :: Snoc hd -> Int
snocLen = foldFix alg
  where
    alg ::  𝘗ᵣAlgebra Int a
    alg (Pᵣ Nothing) = 0
    alg (Pᵣ (Just (counter, _))) = counter + 1

snocString :: (Show hd) => Snoc hd -> String
snocString = foldFix alg
  where
    alg (Pᵣ Nothing) = "()"
    alg (Pᵣ (Just (s, i))) = s ++ " ++ " ++ show i

scanify :: 𝘗ᵣAlgebra state value -> 𝘗ᵣAlgebra (Snoc state) value
scanify alg (Pᵣ Nothing) = nil ⧺ alg (Pᵣ Nothing)
scanify alg (Pᵣ (Just ( accum , val))) = accum ⧺ alg (Pᵣ (Just (s0, val)))
  where
    s0 = snocHead accum

scanFix :: 𝘗ᵣAlgebra state value -> Data.Fix.Fix (𝘗ᵣ value) -> Snoc state
scanFix alg = foldFix (scanify alg)
