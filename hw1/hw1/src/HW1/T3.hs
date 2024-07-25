module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)


data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (s, _) _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, d) _ _ _) = d

tmember :: Ord a => a -> Tree a -> Bool
tmember a Leaf = False
tmember a (Branch _ left head right) 
    | a < head  = tmember a left
    | a > head  = tmember a right
    | a == head = True

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a tree@(Branch (size, height) left head right)
    | a < head  = tBalance $ mkBranch (tinsert a left) head right
    | a > head  = tBalance $ mkBranch left head (tinsert a right)
    | a == head = tree

tFromList :: Ord a => [a] -> Tree a 
tFromList a = foldr tinsert Leaf a

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch Leaf a Leaf                                                         = (Branch (1, 1) Leaf a Leaf)
mkBranch Leaf a tree@(Branch (s, h) l2 b2 r2)                                = (Branch ((s + 1), (h + 1)) Leaf a tree)
mkBranch tree@(Branch (s, h) l1 b1 r1) a Leaf                                = (Branch ((s + 1), (h + 1)) tree a Leaf)
mkBranch tree1@(Branch (s1, h1) l1 b1 r1) a tree2@(Branch (s2, h2) l2 b2 r2) = (Branch ((s1 + s2 + 1), (max h1 h2) + 1) tree1 a tree2)



tBalance :: Ord a => Tree a  -> Tree a
tBalance tree@(Branch (size, height) left head right)
    | tDiff == 2  && rTDiff < 0   = rL (mkBranch left head (rR right))
    | tDiff == 2                  = rL tree
    | tDiff == -2 && lTDiff > 0   = rR (mkBranch (rL left) head right)
    | tDiff == -2                 = rR tree
    | otherwise                   = tree
    where 
      tDiff  = tHeightDifference right left
      rTDiff = tHeightDifference (rightTree right) (leftTree right)
      lTDiff = tHeightDifference (rightTree left) (leftTree left)
  


tHeightDifference :: Tree a -> Tree a -> Int
tHeightDifference r l = tdepth r - tdepth l 

rL :: Ord a => Tree a -> Tree a
rL Leaf = Leaf
rL (Branch _ left head right) = (mkBranch  
                                    (mkBranch left head (leftTree right)) 
                                    (headTree right) 
                                    (rightTree right))

rR :: Ord a => Tree a -> Tree a
rR Leaf = Leaf
rR (Branch _ left head right) = (mkBranch  
                                    (leftTree left) 
                                    (headTree left) 
                                    (mkBranch (rightTree left) head right))

leftTree :: Tree a -> Tree a
leftTree Leaf = Leaf
leftTree (Branch _ l _ _) = l

rightTree :: Tree a -> Tree a
rightTree Leaf = Leaf
rightTree (Branch _ _ _ r) = r

headTree :: Ord a => Tree a -> a
headTree Leaf = error "something wrong, you don't need it during rotate"
headTree (Branch _ _ h _ ) = h