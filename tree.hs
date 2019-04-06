import Data.Tree
import Data.Tree.Pretty

type Direction = Int
type Directions = [Direction]
type Breadcrumbs a = [(Direction, Tree a)]
type Zipper a = (Tree a, Breadcrumbs a)

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

{- To insert I need a empty list to start with, that is why there is a helper
function, to avoid having to give the empty list when used. There might be a
better way of doing this REVISE-}
insertAt :: Tree a -> Int -> Tree a -> Tree a
insertAt subTree i (Node x childNodes) =
    insertAtHelper [] subTree i (Node x childNodes)

insertAtHelper :: [Tree a] -> Tree a -> Int -> Tree a -> Tree a
insertAtHelper prevNodes subTree 0 (Node x childNodes) =
    Node x $ prevNodes ++ [subTree] ++ childNodes
insertAtHelper prevNodes subTree i (Node x (node:nodes)) =
    insertAtHelper (prevNodes ++ [node]) subTree (i - 1) (Node x nodes)

moveDown :: Zipper a -> Directions -> Zipper a
moveDown (Node x childNodes, bs) [] = (Node x childNodes, bs)
moveDown (Node x childNodes, bs) (dir:dirs) =
  let subTree = deleteN dir childNodes
      newTree = Node x subTree
  in moveDown (childNodes !! dir, (dir, newTree):bs) dirs

moveUp :: Zipper a -> Int -> Zipper a
moveUp (tree, bs) 0 = (tree, bs)
moveUp (tree, bs) levels =
  let index   = fst . head $ bs
      parent  = snd . head $ bs
      newTree = insertAt tree index parent
  in moveUp (newTree, tail bs) (levels-1)

tree :: Tree String
tree = Node "hello" [ Node "foo" []
                     , Node "bars" [ Node "oi!" []
                                   , Node "baz" [ Node "a" [ Node "b" []
                                                           , Node "c" []]
                                                , Node "d" [ Node "e" []]]]
                     , Node "foobar" []]
