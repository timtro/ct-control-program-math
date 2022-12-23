{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE UnicodeSyntax #-}

module PrAlgebra where

import           Data.Fix (Fix (Fix), foldFix, unFix)

type GlobalElement a = () → a

makeGlobal :: a → GlobalElement a
makeGlobal = const
-- makeGlobal val = \_ -> val

newtype 𝘗ᵣ hd tl = Pᵣ (Maybe (tl, hd))

instance Functor (𝘗ᵣ hd) where
  fmap :: (a → b) → 𝘗ᵣ hd a → 𝘗ᵣ hd b
  fmap f (Pᵣ Nothing)         = Pᵣ Nothing
  fmap f (Pᵣ (Just (tl, hd))) = Pᵣ (Just (f tl, hd))

type 𝘗ᵣAlgebra state value =  𝘗ᵣ value state → state

(ge ▽ f) x = case x of
  (Pᵣ Nothing)         -> ge()
  (Pᵣ (Just (tl, hd))) -> f (tl, hd)

(△) :: (b → c) → (b → c') → b → (c, c')
(△) f g x = (f x, g x)

type Snoc hd = Fix(𝘗ᵣ hd)

snoc :: Snoc a → a → Snoc a
snoc xs x = Fix (Pᵣ (Just (xs, x)))

nil :: Snoc a
nil = Fix (Pᵣ Nothing)

(⧺) :: Snoc a → a → Snoc a
(⧺) = snoc

snocHead :: Snoc hd → hd
snocHead (Fix (Pᵣ Nothing)) = error "snocHead called on empty Snoc List"
snocHead (Fix (Pᵣ( Just (xs, x) ) ) ) = x

scanify :: 𝘗ᵣAlgebra state value → 𝘗ᵣAlgebra (Snoc state) value
scanify alg (Pᵣ Nothing) = nil ⧺ alg (Pᵣ Nothing)
scanify alg (Pᵣ (Just ( accum , val))) = accum ⧺ alg (Pᵣ (Just (s0, val)))
  where
    s0 = snocHead accum

scanFix :: 𝘗ᵣAlgebra state value → Data.Fix.Fix (𝘗ᵣ value) → Snoc state
scanFix alg = foldFix (scanify alg)

snocToList :: Snoc hd → [hd]
snocToList = foldFix alg
  where
    -- alg :: 𝘗ᵣ hd [hd] → [hd]
    alg :: 𝘗ᵣAlgebra [hd] hd
    alg (Pᵣ Nothing)             = []
    alg (Pᵣ (Just (accum, val))) = accum ++ [val]

listToSnoc :: [a] → Snoc a
listToSnoc = foldl snoc nil

snocLen :: Snoc hd → Int
snocLen = foldFix alg
  where
    alg ::  𝘗ᵣAlgebra Int a
    alg (Pᵣ Nothing)             = 0
    alg (Pᵣ (Just (counter, _))) = counter + 1

snocString :: (Show hd) ⇒ Snoc hd → String
snocString = foldFix alg
  where
    alg :: (Show hd) ⇒ 𝘗ᵣAlgebra String hd
    alg (Pᵣ Nothing)       = "()"
    alg (Pᵣ (Just (s, i))) = s ++ " ++ " ++ show i
