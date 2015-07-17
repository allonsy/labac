{- Alec Snyder
- labac wrapper
- structures and alebraic data type definitions -}
--module Structs where

--packageTree: a basic rose tree made to represent a package at the node
--and dependent packages in the leaves (represented as a list)
data PackageTree a = Node a [PackageTree a]
  deriving(Show)

instance Functor PackageTree where
  fmap f (Node val vals) = Node (f val) (map (fmap f) vals)

--listByPredicate: given a package tree, and a predicate, it returns all
--packages that satisfy that predicate in the form of a list 
listByPredicate :: (a -> Bool) -> PackageTree a -> [a]
listByPredicate p (Node val vals)
  | p val =  val : (vals >>= listByPredicate p)
  | otherwise = vals >>= listByPredicate p

a = Node 1 [Node 2 [], Node 3 [], Node 4 [Node 5 [Node 7 []], Node 6[]]]
