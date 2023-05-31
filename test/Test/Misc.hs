{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Misc
    ( miscSpec, main
    ) where

import Data.Char (chr, ord)
import Debug.Trace
import Test.Hspec (Spec, describe, hspec, it, shouldBe)


myId::a -> a
myId x = x

even':: Integral a=> a->  Bool
-- even' x = mod x 2 == 0
even' 0 = True
even' 1 = False
even' n = not $ even (n-1)

signum':: (Ord a, Num a )=> a -> a
signum' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

halve::Show a => [a]->([a], [a])
halve xs = trace(show xs ++ " n: " ++ show n) (take n xs, drop n xs)
  where
    n = length xs `div` 2

luhn::String -> Bool
luhn s = fn(reverse $ filter (/= ' ') s, 1, 0)
  where
    dbl x = let n = 2*x in
      if n > 9 then n-9 else n

    fn("",_,acc)     = acc `mod` 10 == 0
    fn((x:xs),i,acc)
      | even i = fn(xs, i+1, acc + dbl ( read [x]))
      | otherwise = fn(xs, i+1, acc + ( read [x]))


zip'::[a]->[b]->[(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

sorted::Ord a=> [a] -> Bool
sorted xs = and [x<=y | (x,y) <- zip xs (tail xs)]

positions::Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, x') <- zip [0..] xs, x == x']

-- Caesar cipher
encode::String -> Int -> String
encode xs n = [enc x| x <- xs]
  where
    enc c
      | c >= 'a' && c <= 'z' = chr $ ord 'a' + ((ord c - ord 'a' + n) `mod` 26 )
      | otherwise = c

decode::String -> Int -> String
decode xs n = encode xs (-n)

count::String-> Char -> Int
count xs x = sum [1 | x' <- xs, x'==x]

lowers::String -> String
lowers = filter (\x-> x >= 'a' && x <= 'z')

sFrq:: String -> [Float]
sFrq xs = [fromIntegral (count xs x) / n | x <- ['a'..'z']]
  where n = fromIntegral $ length $ lowers xs

chisqr::[Float] -> [Float] -> Float
-- chisqr xs ys = sum [(x-y)**2 | (x,y) <- zip xs ys]
chisqr xs ys = sum [(x-y)**2 / x | (x,y) <- zip xs ys]

shift:: [a] -> Int -> [a]
shift xs n = drop k xs ++ take k xs
  where k = n `mod` length xs

argmin::Ord a => [a] -> Int
argmin xs = (positions (minimum xs) xs) !! 0

crack:: String -> String
crack xs = decode xs n
  where
    table :: [Float]
    table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
              0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
              6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

    sfr = sFrq xs
    n = argmin [chisqr table (shift sfr i)  | i <- [0..25]]

takewhile::(a->Bool)-> [a] -> [a]
takewhile _ [] = []
takewhile f (x:xs)
  | f x = x : takewhile f xs
  | otherwise = []

primes::[Int]
primes = 2 : [x | x<-[3..], isPrime x]
  where isPrime x = and [x `mod` p /= 0 | p <- takewhile (<= x `div` 2) primes]
  -- where isPrime x = and [x `mod` p /= 0 | p <- [2..x-1]]

cipherSpec :: Spec
cipherSpec = describe "Misc" $ do
    describe "excercise 5" $ do
      it "ex 7" $ concat [[(x,y) | y <- [3,4]] | x <- [1,2]] `shouldBe`
                        [(x,y) | x <- [1,2], y <- [3,4]]

      it "takewhile" $ takewhile (< 5) [1,3,5,6] `shouldBe` [1,3]
      it "primes" $ take 10 primes `shouldBe` [2,3,5,7,11,13,17,19,23,29]

    describe "crack" $ do
        let s = "haskell is fun"
          in do
            it (s ++ " 3") $ crack (encode s 3) `shouldBe` s
            it (s ++ " 10") $ crack (encode s 10) `shouldBe` s

        let s = "fox is strange animal"
          in do
            it (s ++ " 3") $ crack (encode s 3) `shouldBe` s
            it (s ++ " 10") $ crack (encode s 10) `shouldBe` s


    describe "cipher break" $ do
        it "count" $ count "abcaa" 'a' `shouldBe` 3
        it "count b" $ count "abca" 'b' `shouldBe` 1
        it "sFrq" $ take 2 (sFrq "abcaa") `shouldBe` [3/5, 1/5]
        it "sFrq" $ take 3 (sFrq "ab Cc aa") `shouldBe` [3/5, 1/5, 1/5]
        it "chisqr" $ chisqr [1,2] [2,2] `shouldBe` 1
        it "shift" $ shift [1,2,3] 1 `shouldBe` [2,3,1]
        it "argmin" $ argmin [1,2,3] `shouldBe` 0
        it "argmin" $ argmin [1,2,0,3] `shouldBe` 2

    describe "Caesar" $ do
        it "3" $ encode "haskell is fun z" 3 `shouldBe` "kdvnhoo lv ixq c"
        it "10" $ encode "haskell is fun" 10 `shouldBe` "rkcuovv sc pex"
        it "encode, decooe" $ decode (encode "haskell is fun" 10) 10 `shouldBe` "haskell is fun"

    describe "positions" $ do
        it "Int 1, 3" $ positions 1 [2, 1, 3, 1, 5] `shouldBe` [1, 3]
        it "Bool 1, 3" $ positions False [True, False, True, False] `shouldBe` [1, 3]

miscSpec :: Spec
miscSpec = describe "Misc" $ do

    describe "sorted" $ do
        it "ok" $ sorted [1, 3, 10] `shouldBe` True
        it "not ok" $ sorted [1, 0, 10] `shouldBe` False

    describe "zip" $ do
        it "xs < ys" $ zip' [1,2] [1,2,3] `shouldBe` [(1,1),(2,2)]
        it "xs > ys" $ zip' [1,2,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(4,3)]
        it "xs == ys" $ zip' [1,2,4] [1,2,3] `shouldBe` [(1,1),(2,2),(4,3)]

    describe "obvious myId" $ do
        it "1 = 1" $ 1 `shouldBe` 1
        it "id" $ myId 1 `shouldBe` 1
        it "id" $ myId "foo" `shouldBe` "foo"
        it "id" $ myId 'a' `shouldBe` 'a'

    describe "even'" $ do
        it "0" $ even' 0 `shouldBe` True
        it "4" $ even' 4 `shouldBe` True
        it "3" $ even' 3 `shouldBe` False

    describe "signum" $ do
        it "0" $ signum' 0 `shouldBe` 0
        it "-3" $ signum' (-3) `shouldBe` -1
        it "3" $ signum' (3) `shouldBe` 1

    describe "halve" $ do
      it "lists eq" $ [1,2] `shouldBe` [1,2]
      it "lists !!" $ [1,2] !! 1 `shouldBe` 2
      it "even" $ halve [1,2,3,4,5,6] `shouldBe` ([1,2,3],[4,5,6])
      it "odd" $ halve [1,2,3,4,5,6,7] `shouldBe` ([1,2,3],[4,5,6,7])

    describe "luhn" $ do
      it "1784" $ luhn "1784" `shouldBe` True
      it "4783" $ luhn "4783" `shouldBe` False
      it "4556 7375 8689 9855" $ luhn "4556 7375 8689 9855" `shouldBe` True
      it "4556737586899854" $ luhn "4556737586899854" `shouldBe` False


main::IO()
main = do
  -- hspec miscSpec
  hspec cipherSpec


