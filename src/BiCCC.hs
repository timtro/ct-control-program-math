{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}

module BiCCC where

type GlobalElement a = () → a

makeGlobal :: a → GlobalElement a
makeGlobal = const
-- makeGlobal val = \_ -> val

-- (▽) :: (a → c) → (b → c) → Either a b → c
-- (f ▽ g) (Left x) = f x
-- (f ▽ g) (Right x) = g x

(△) :: (b → c) → (b → c') → b → (c, c')
(△) f g x = (f x, g x)
