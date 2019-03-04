{-
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.Serialize
-}
import Data.Hashable
import GHC.Generics

data Tree t = Leaf
            | Node (Tree t) t (Tree t)

sumTree Leaf = 0
sumTree (Node left v right) =
  v + sumTree left + sumTree right
