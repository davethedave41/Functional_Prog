{- urdaibad David Urdaibay -}
module Ex03 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Binary Tree
data BT a b
  = Leaf
  | Branch (BT a b) a b (BT a b)
  deriving (Eq, Show)

-- association list
-- [(a,b)] :: ([(a,b)]) associates values of type 'a' with those of type 'b'
type Assoc a b = [(a,b)]

-- lookup binary (search) tree
lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
lkpBST Leaf _  =  Nothing
lkpBST (Branch left k d right) k'
 | k < k'     =  lkpBST left k'
 | k > k'     =  lkpBST right k'
 | otherwise  =  Just d

-- Coding Part 1 (13 Marks)

-- insert into binary (search) tree
insBST :: Ord a => a -> b -> BT a b -> BT a b
insBST p n Leaf = (Branch Leaf p n Leaf)
insBST p n (Branch l k d r)
 | p < k      = (Branch (insBST p n l) k d r)
 | p > k      = (Branch l k d (insBST p n r))
 | otherwise  = (Branch l k n r)
-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
-- k & d and keys -> datas
assoc2bst :: Ord a => Assoc a b -> BT a b
assoc2bst [] = Leaf
assoc2bst ((k,d):rest) = (insBST k d (assoc2bst rest))
{- 
   Actual fluke but lemme explain before I forget what I did (for above): insert the current key
   and data value (using insBST) that uses assoc2bst function as the BST input
   because the function has output of BST so in the end the assoc2bst will output the BST 
-}
--
-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
bst2assoc :: Ord c =>  BT c e -> Assoc c e
bst2assoc Leaf = []
bst2assoc (Branch left k d right) = concat[bst2assoc left, [(k,d)],bst2assoc right]

