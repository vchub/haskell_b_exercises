{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bin
    ( main
    ) where

import Data.Char (chr, ord)
import Data.List (sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Bin = Int


bin2int0::[Bin] -> Int
bin2int0 = fst . foldl (\(num, p) x -> ((num + x*p), p*2)) (0, 1)

-- bin2int::[Bin] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--   where weights = iterate (*2) 1

bin2int::[Bin] -> Int
bin2int = foldr (\x acc -> x + acc*2) 0

int2bin:: Int -> [Bin]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

-- prop_int2bin::Int -> Bool
-- prop_int2bin n = bin2int (int2bin n) == n &&  bin2int0 (int2bin n) == n

make8::[Bin] -> [Bin]
make8 xs = take 8 (xs ++ repeat 0)

take8::[Bin] -> [[Bin]]
take8 [] = []
take8 xs = take 8 xs : take8 (drop 8 xs)

encode::String -> [Bin]
encode = concat . map (make8 . int2bin . ord)
-- encode = concat . map (\c-> make8 (int2bin (ord c)))

decode::[Bin] -> String
decode = map (chr . bin2int) . take8
-- decode xs = foldl (\acc byte -> chr (bin2int byte) : acc) ""  (take8 xs)

prop_encode::String -> Bool
prop_encode s = decode (encode s') == s'
  where s' = filter (\c-> c >= 'a' && c <= 'z') s

count::Eq a => a-> [a]-> Int
count x = length . filter (== x)

rmdups::Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result xs = sort [(count x xs, x) | x <- rmdups xs]

-- exercises
curry'::((a,b)->c) -> a->b->c
curry' f = \a -> \b -> f (a,b)

uncurry':: (a->b->c) -> ((a,b)->c)
uncurry' f = \(a,b) -> f a b


altMap::(a->b) ->  (a->b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs


spec::Spec
spec = describe "Bin" $ do
    describe "altMap" $ do
      it "book ex" $ altMap (+10) (+100) [0,1,2,3,4] `shouldBe` [10,101,12,103,14]

    describe "curry'" $ do
      let f = \(a,b) -> a+b in it "curry'" $ (curry' f) 1 2 `shouldBe` 3
      let f = \a b -> a+b in it "uncurry'" $ (uncurry' f) (1,2) `shouldBe` 3

    describe "voting" $ do
      it "count 0" $ count 0 [0,1,0] `shouldBe` 2
      it "rmdups 0" $ rmdups [0,1,0] `shouldBe` [0,1]
      it "result" $ result [0,1,0] `shouldBe` [(1,1), (2, 0)]
      it "result String" $ result ["Red", "Blue", "Green", "Blue", "Blue", "Red"] `shouldBe` [(1,"Green"), (2,"Red"), (3,"Blue")]

    describe "encode decode" $ do
      it "a" $ decode (encode "a") `shouldBe` "a"
      let s = "abc" in it s $ decode (encode s) `shouldBe` s
      let s = "a abc" in it s $ decode (encode s) `shouldBe` s

    describe "bin2int0" $ do
      it "0" $ bin2int0 [0] `shouldBe` 0
      it "5" $ bin2int0 [1,0,1] `shouldBe` 5
      let xs = [1,1,1] in it (show xs) $ bin2int0 xs `shouldBe` bin2int xs

      let n = 3 in it ("both " ++ show n) $ bin2int (int2bin n) `shouldBe` n

main::IO()
main = do
  hspec spec
  -- quickCheck prop_int2bin
  quickCheck prop_encode

  putStrLn "DONE tests"
