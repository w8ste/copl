module Assignment where

import Data.List

-- Below is a definition of a Collection type class that defines some operations
-- which are applicable to typical collections such as lists, sets or trees:
--
--   - hasElem e c : tests if e is an element of the collection c
--   - size c : computes the number of elements in the collection c
--   - allElem c : returns all elements of the collection c as a list
--
-- Below you can also see an example of an instance declaration of this type
-- class for lists.

class Collection c where
  hasElem :: Eq a => a -> c a -> Bool
  size :: Eq a => c a -> Int
  allElem :: Eq a => c a -> [a]

instance Collection [] where
  hasElem e lst = elem e lst
  size lst = length lst
  allElem lst = lst

test4_ =
  [
    hasElem 2 [1, 2, 3] == True,
    hasElem 4 [1, 2, 3] == False,
    size ['a', 'b', 'c'] == 3,
    sort (allElem ["a", "b", "c"]) == ["a", "b", "c"]
  ]

-- **** Task 4a ****

data BinTree a = NullNode
               | BinNode a (BinTree a) (BinTree a)

-- Implement an instance declaration that makes the data type BinTree, which
-- describes binary trees, an instance of the Collection type class.
--
-- (Please note that the tree does not need to be sorted in any way; "hasElem"
-- only requires "Eq a", not "Ord a". Thus, our binary trees need not be binary
-- *search* trees.)

{-
instance Collection BinTree where ...
-}

-- **** Task 4b ****
--
-- Implement the function "subset" which takes two collections with the same
-- types of elements and checks if the first collection is a subset of
-- the second, i.e. if all elements of the first collection are present
-- in the second one.

subset :: (Eq a, Collection c1, Collection c2) => c1 a -> c2 a -> Bool
subset c1 c2 = undefined

-- **** Tests ****
-- uncomment and run the following tests:


{-
test4a = let
    tree1 = BinNode 3 (BinNode 1 NullNode NullNode) (BinNode 2 NullNode NullNode)
    tree2 = BinNode "a" (BinNode "b" NullNode NullNode) (BinNode "c" NullNode NullNode)
  in [
    hasElem 2 tree1 == True,
    hasElem 4 tree1 == False,
    size tree2 == 3,
    sort (allElem tree2) == ["a", "b", "c"]
  ]

test4b = let
    tree1 = BinNode 3 (BinNode 1 NullNode NullNode) (BinNode 2 NullNode NullNode)
    tree2 = BinNode "a" (BinNode "b" NullNode NullNode) (BinNode "c" NullNode NullNode)
  in [
    subset tree1 [2, 3, 4, 1] == True,
    subset [1, 4] tree1 == False,
    subset ["a", "a", "b"] tree2 == True,
    subset [2, 2, 1, 1] tree1 == True
  ]

main = do
  print test4_
  print test4a
  print test4b

-}
