-- CPSC 312 - 2018 - Binary Search Trees in Haskell
-- Copyright D. Poole 2018, released under the GPL.

module BSTree where

-- To run it, try:
-- ghci
-- :load BSTree

-- a binary search tree
data BSTree k v = Empty
                | Node k v (BSTree k v) (BSTree k v)

-- tolist tree  returns the list giving the inorder traversal of tree
tolist :: BSTree k v -> [(k,v)]
tolist Empty = []
tolist (Node key val lt rt) =
     tolist lt ++ ((key,val) : tolist rt)

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolist atree





-- tolist without append (++), using accumulators
tolista :: BSTree k v -> [(k,v)]
tolista lst =
    tolist2 lst []

-- tolist2 tree lst   returns the the list of elements of tree followed by the elements of lst
-- tolist2 :: BSTree k v -> [(k,v)] -> [(k,v)]
tolist2 Empty acc = acc
tolist2 (Node key val lt rt) acc =
     tolist2 lt  ((key,val) : tolist2 rt acc)

--- Example to try
-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista atree






-- insert key val tree   returns the tree that results from inserting key with value into tree
insert :: Ord k => k -> v -> BSTree k v -> BSTree k v
insert key val Empty = Node key val Empty Empty
insert key val (Node k1 v1 lt rt)
    | key == k1 = Node key val lt rt
    | key < k1 = Node k1 v1 (insert key val lt) rt
    | key > k1 = Node k1 v1 lt (insert key val rt)

-- atree =  Node 5 "alive" (Node 2 "be" Empty (Node 4 "fun" Empty Empty)) (Node 8 "food" Empty  Empty)
-- tolista (insert 6 "six" atree)

-- what if we wanted to return the old value as well as the tree?
-- what if there wasn't an old value; what should be returned? (It has to be of the correct type!)
