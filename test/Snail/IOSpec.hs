module Snail.IOSpec where

import Data.Either
import Snail.Shell
import Test.Hspec

spec :: Spec
spec = do
    describe "successfully lexes some basic snail files" $ do
        it "lex a basic snail file" $ do
            eResults <- readSnailFile "snail-files/basic.snail"
            eResults `shouldSatisfy` isRight

        it "lex an empty snail file" $ do
            eResults <- readSnailFile "snail-files/fail-empty.snail"
            eResults `shouldSatisfy` isLeft

        it "lex should fail with bad comment" $ do
            eResults <- readSnailFile "snail-files/fail-comment.snail"
            eResults `shouldSatisfy` isLeft

        it "lex should fail with non-terminated s-expression" $ do
            eResults <- readSnailFile "snail-files/fail.snail"
            eResults `shouldSatisfy` isLeft

        it "lex should fail with non-escaped quote" $ do
            eResults <- readSnailFile "snail-files/fail-quotes.snail"
            eResults `shouldSatisfy` isLeft

        it "lex should fail on naked nil" $ do
            eResults <- readSnailFile "snail-files/fail-nil.snail"
            eResults `shouldSatisfy` isLeft

    describe "successfully lexes some examples from other languages" $ do
        it "lexes a basic scheme file with converted comments" $ do
            eResults <- readSnailFile "./test/files/r5rs_pitfalls.scm"
            eResults `shouldSatisfy` isRight

        it "lexes a basic fennel file with converted comments" $ do
            eResults <- readSnailFile "./test/files/fennel-reference.fnl"
            eResults `shouldSatisfy` isRight
