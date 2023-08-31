{-# LANGUAGE QuasiQuotes #-}

module Snail.LexerSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text
import Snail
import Test.HUnit (assertBool)
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ

foldLexemes :: SnailAst -> [Text]
foldLexemes = go []
  where
    go :: [Text] -> SnailAst -> [Text]
    go acc (Lexeme (_, t)) = acc ++ [t]
    go acc (TextLiteral (_, t)) = acc ++ [t]
    go acc (SExpression _ _ []) = acc
    go acc (SExpression _ _ (x : xs)) = lgo (go acc x) xs
    lgo :: [Text] -> [SnailAst] -> [Text]
    lgo acc [] = acc
    lgo acc (x : xs) = lgo (go acc x) xs

failAssertion :: String -> Expectation
failAssertion s = assertBool s False

sExpressionShouldBe :: Text -> [Text] -> Expectation
sExpressionShouldBe input output =
    case parseMaybe sExpression input of
        Nothing -> failAssertion "sExpressionShouldBe: Nothing"
        Just sExpr -> do
            let lexemes = foldLexemes sExpr
            lexemes `shouldBe` output

textLiteralShouldBe :: Text -> [Text] -> Expectation
textLiteralShouldBe input output =
    case parseMaybe textLiteral input of
        Nothing -> failAssertion "textLiteralShouldBe: Nothing"
        Just sExpr -> do
            let lexemes = foldLexemes sExpr
            lexemes `shouldBe` output

spec :: Spec
spec = do
    describe "non quote character" $ do
        it "handles spaces properly" $ do
            let mSExpr = parseMaybe nonQuoteCharacter " "
            mSExpr `shouldBe` Just " "

    describe "parse text literals" $ do
        it "successfully parses a basic text literal" $ do
            [r|"hello \"world"|] `textLiteralShouldBe` [[r|hello \"world|]]

        it "successfully parses a text literal with leading/trailing quotes" $ do
            [r|"\"hello \"world\""|] `textLiteralShouldBe` [[r|\"hello \"world\"|]]

        it "fails to lex text literal with unescaped quote" $ do
            let mSExpr = parseMaybe textLiteral [r|"hello "world"|]
            mSExpr `shouldSatisfy` isNothing

        it "can parse the empty string" $ do
            [r|""|] `textLiteralShouldBe` [""]

        it "can parse whitespace" $ do
            [r|" "|] `textLiteralShouldBe` [" "]

        it "can parse more whitespace" $ do
            [r|"    "|] `textLiteralShouldBe` ["    "]

    describe "parse sExpression" $ do
        it "successfully lex a basic list" $ do
            "(a b c)" `sExpressionShouldBe` ["a", "b", "c"]

        it "successfully parse nil inside parentheses" $ do
            "(nil)" `sExpressionShouldBe` ["nil"]

        it "fail to parse a standalone nil" $ do
            let mSExpr = parseMaybe sExpression "nil"
            mSExpr `shouldSatisfy` isNothing

        it "successfully lex a basic list" $ do
            "(1 a)" `sExpressionShouldBe` ["1", "a"]

        it "successfully lex a basic list with starting character" $ do
            "'(1 a)" `sExpressionShouldBe` ["1", "a"]

        it "successfully lex a single element list" $ do
            "(1a)" `sExpressionShouldBe` ["1a"]

        it "successfully lex a nested s-expression" $ do
            "((1a))" `sExpressionShouldBe` ["1a"]

        it "successfully lex a nested s-expression with different brackets" $ do
            "([1a])" `sExpressionShouldBe` ["1a"]

        it "successfully lex nested s-expressions" $ do
            "(() ())" `sExpressionShouldBe` []

        it "successfully lex nested s-expressions of each bracket" $ do
            "(() [] <> {})" `sExpressionShouldBe` []

        it "successfully lex internally nested s-expressions of each bracket" $ do
            "([<{}>])" `sExpressionShouldBe` []

        it "successfully lex a nested s-expressions" $ do
            "((()) (()))" `sExpressionShouldBe` []

        it "successfully lex a nested s-expressions of different brackets" $ do
            "(<()> <{}>)" `sExpressionShouldBe` []

        it "successfully lex line comment" $ do
            "(-- ...\n)" `sExpressionShouldBe` []

        it "successfully lex line comment followed by token" $ do
            "(-- ...\nabc)" `sExpressionShouldBe` ["abc"]

        it "successfully lex line comment with \r\n followed by token" $ do
            "(-- ...\r\nabc)" `sExpressionShouldBe` ["abc"]

        it "successfully lex line comment with \t followed by token" $ do
            "(-- ...\n\tabc)" `sExpressionShouldBe` ["abc"]

        it "successfully lex line comment with \v followed by token" $ do
            "(-- ...\n\vabc)" `sExpressionShouldBe` ["abc"]

        it "successfully lex block comment" $ do
            "({- ... -})" `sExpressionShouldBe` []

        it "successfully lex block comment followed by token" $ do
            "({- ... -}abc)" `sExpressionShouldBe` ["abc"]

        it "successfully lex token followed by block comments" $ do
            "(abc{- ... -})" `sExpressionShouldBe` ["abc"]

        it "successfully lex block comments sorrounded by tokens" $ do
            "(abc{- ... -}def)" `sExpressionShouldBe` ["abc", "def"]

        it "successfully lex nested block comments" $ do
            "({- ... {- ... -} ... -})" `sExpressionShouldBe` []

        it "fail to lex nested block comments with missing internal start" $ do
            parseMaybe sExpression "({- ... -} ... -})" `shouldBe` Nothing

        it "fail to lex nested block comments with missing internal stop" $ do
            parseMaybe sExpression "({- ... {- ... -})" `shouldBe` Nothing

        it "fail to lex block comment with missing stop" $ do
            parseMaybe sExpression "({- ...)" `shouldBe` Nothing

        it "can handle subsequent s-expressions" $ do
            parseMaybe snailAst "()(nil)()" `shouldSatisfy` isJust

        it "fails to parse nested naked nil" $ do
            parseMaybe snailAst "()nil()" `shouldSatisfy` isNothing

        it "handles successive text literals" $ do
            [r|("hello" " " "world" "!!!")|] `sExpressionShouldBe` ["hello", " ", "world", "!!!"]

        it "fails to parse nested mismatched brackets" $ do
            parseMaybe snailAst "([)]" `shouldSatisfy` isNothing
