{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Misc
    ( miscSpec, main
    ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)


myId::a -> a
myId x = x

miscSpec :: Spec
miscSpec = describe "Misc" $ do
    describe "obvious myId" $ do
        it "1 = 1" $ 1 `shouldBe` 1
        it "id" $ myId 1 `shouldBe` 1
        it "id" $ myId "foo" `shouldBe` "foo"
        it "id" $ myId 'a' `shouldBe` 'a'


main::IO()
main = hspec miscSpec


