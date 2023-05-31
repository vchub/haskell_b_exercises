{-# OPTIONS_GHC -Wno-type-defaults #-}

module Types
    ( main
    ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

data Nat = Zero | Succ Nat deriving (Eq, Show)

nat2int::Nat -> Int
nat2int Zero     = 0
nat2int (Succ x) = 1 + (nat2int x)

int2nat::Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add::Nat -> Nat -> Nat
add Zero n     = n
add (Succ n) m = Succ (add n m)

sub::Nat -> Nat -> Nat
sub n Zero            = n
sub Zero _            = Zero
sub (Succ n) (Succ m) = sub n m

data Tree a = Leaf | Node (Tree a) a (Tree a)

occurs::Ord a => a -> Tree a -> Bool
occurs _ Leaf = False
occurs x (Node l y r)
    | x==y = True
    | x < y = occurs x l
    | x > y = occurs x r
    |otherwise = False

insert::Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r)
  | x < y  = Node (insert x l) y r
  | x > y  = Node l y  (insert x r)
  | otherwise = Node l y r


fromArr::Ord a => [a] -> Tree a
fromArr = foldl (\acc x -> insert x acc) Leaf

flatten::Tree a -> [a]
flatten Leaf         = []
flatten (Node l x r) = (flatten l) ++ [x] ++ flatten r

spec::Spec
spec = describe "Types" $ do
    describe "Tree" $ do
      it "occurs" $ occurs 2 (fromArr [3,1,2,4]) `shouldBe` True
      it "occurs" $ occurs 0 (fromArr [3,1,2,4]) `shouldBe` False
      it "fromArr, flatten" $ flatten (fromArr [3,1,2,4]) `shouldBe` [1,2,3,4]

    describe "Nat" $ do
      it "3" $ int2nat 3 `shouldBe` Succ (Succ (Succ Zero))
      it "3 both" $ nat2int (int2nat 3) `shouldBe` 3
      it "3 + 0" $ nat2int (add (int2nat 3) (int2nat 0)) `shouldBe` 3
      it "3 + 2" $ nat2int (add (int2nat 3) (int2nat 2)) `shouldBe` 5
      it "3 - 2" $ nat2int (sub (int2nat 3) (int2nat 2)) `shouldBe` 1
      it "3 - 4" $ nat2int (sub (int2nat 3) (int2nat 4)) `shouldBe` 0

main::IO()
main = do
  hspec spec
