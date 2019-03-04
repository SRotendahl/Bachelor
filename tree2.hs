import Data.Tree
import Data.Tree.Pretty
tree :: Tree String
tree = Node "hello" [ Node "foo" []
                     , Node "bars" [ Node "oi!" []
                                   , Node "baz" [ Node "a" [ Node "b" []
                                                           , Node "c" []]
                                                , Node "d" [ Node "e" []]]]
                     , Node "foobar" []]
