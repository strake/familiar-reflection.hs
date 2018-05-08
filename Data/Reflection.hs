module Data.Reflection (Reifier (..), reify) where

import Data.Proxy
import Unsafe.Coerce

class Reifier s where
    type Res s
    reflect :: proxy s -> Res s

newtype Magic a b = Magic (∀ (s :: *) . (Reifier s, Res s ~ a) => Proxy s -> b)

reify :: ∀ a b . a -> (∀ (s :: *) . (Reifier s, Res s ~ a) => Proxy s -> b) -> b
reify a k = unsafeCoerce (Magic k :: Magic a b) (\ _ -> a) Proxy
{-# INLINE reify #-}
