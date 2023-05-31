{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.QuickCheckTutor
    ( main
    ) where

-- import Debug.Trace
import Data.List (intersperse)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- reverse
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs


split::Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = h : split x t
  where
    h = takeWhile (/=x) xs
    t' = dropWhile (/=x) xs
    t | null t' = []
      | otherwise = tail t'

-- unsplit::a-> [[a]] -> [a]
unsplit::Char-> [String] -> String
unsplit c ( "":xs ) = c : unsplit c xs
unsplit c xs        = concat $ intersperse [c] xs

prop_split:: String -> Bool
prop_split xs = and $ map (\x -> unsplit x (split x xs) == xs) xs
-- prop_split xs = forAll (elements xs) $ \x -> unsplit x (split x xs) == xs

sorted::Ord a => [a]->Bool
sorted []     = True
sorted [_]    = True
sorted (x:xs) = x <= head xs && sorted xs

isort::Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x $ isort xs
  where
    insert a [] = [a]
    insert a (b:ys)
      | a <= b = a:b:ys
      | otherwise = b : insert a ys


-- prop_isort::Ord a => [a] -> Bool
prop_isort::[Int] -> Bool
prop_isort xs  = sorted $ isort xs

spec::Spec
spec = describe "Misc" $ do
    describe "isort" $ do
      it "ok" $ sorted ([]::[Int]) `shouldBe` True
      it "ok" $ sorted [2,1] `shouldBe` False
      it "ok" $ sorted [2,3,10] `shouldBe` True
      it "ok" $ isort [2,3,1] `shouldBe` [1,2,3]
      it "ok" $ isort [1,0,0] `shouldBe` [0,0,1]

    describe "split" $ do
      let s = "/" in it (s ++ " " ++ s) $ split '/' s `shouldBe` [""]
      let s = "a" in it s $ split '/' s `shouldBe` ["a"]
      let s = "aa" in it s $ split 'a' s `shouldBe` ["", ""]
      let s = "bac" in it s $ split 'a' s `shouldBe` ["b", "c"]
      let s = "pbv@dcc.fc.up.pt" in it s $ split '@' s `shouldBe` ["pbv","dcc.fc.up.pt"]
      let s = "/usr/include" in it s $ split '/' s `shouldBe` ["", "usr", "include"]
      it "unsplit" $ unsplit '/' ["", "usr", "include"] `shouldBe` "/usr/include"

      it "a" $ split 'a' "a" `shouldBe` [""]
      it "unsplit a" $ unsplit 'a' [""] `shouldBe` "a"
      it "unsplit b" $ unsplit 'a' ["b"] `shouldBe` "b"
      it "unsplit split a ab" $ unsplit 'a' (split 'a' "ab") `shouldBe` "ab"
      it "ab" $ split 'b' "ab" `shouldBe` ["a",""]
      it "unsplit split b ab" $ unsplit 'b' (split 'b' "ab") `shouldBe` "ab"

main::IO()
main = do
  hspec spec
  quickCheck prop_revapp
  quickCheck prop_split
  quickCheck prop_isort
  -- print $ "hi there " ++ show [1,2,3]


