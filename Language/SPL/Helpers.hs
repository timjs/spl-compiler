module Language.SPL.Helpers where

infixr 1 ?

-- | condition ? then $ else
(?) :: Bool -> a -> a -> a
(?) b t f = if b then t else f

