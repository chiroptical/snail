{-# LANGUAGE QuasiQuotes #-}

module Snail.LexerSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text, unpack)
import Snail
import Test.HUnit (assertBool)
import Test.Hspec
import Text.Megaparsec (parseMaybe, parseTest)
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
        Nothing -> failAssertion $ "sExpressionShouldBe: " <> unpack input
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
        it "preserves escaped backslashes and quotes without swallowing the next token" $ do
            [r|("\\" next "\\\"" "\n")|] `sExpressionShouldBe` [[r|\\|], "next", [r|\\\"|], [r|\n|]]

        it "rejects a dangling escape prefix inside an incomplete string literal" $ do
            parseMaybe textLiteral [r|"abc\|] `shouldBe` Nothing

        it "rejects an escaped double quote inside an incomplete string literal" $ do
            parseMaybe textLiteral [r|"abc\"|] `shouldBe` Nothing

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
        it "attaches comma-at to the immediately following list" $
            case parseMaybe sExpression "(list ,@(xs))" of
                Just (SExpression Nothing Round [Lexeme (_, "list"), SExpression (Just ",@") Round [Lexeme (_, "xs")]]) -> pure ()
                result -> expectationFailure $ show result

        it "round-trips a comma-at-prefixed list" $
            case parseMaybe sExpression ",@(xs)" of
                Just ast@(SExpression (Just ",@") Round [Lexeme (_, "xs")]) ->
                    toText ast `shouldBe` ",@(xs)"
                result -> expectationFailure $ "Expected a ,@-prefixed list, got " <> show result

        it "preserves a trailing apostrophe in an identifier" $
            "(foldl')" `sExpressionShouldBe` ["foldl'"]

        it "preserves an at sign inside an identifier" $
            "(user@host)" `sExpressionShouldBe` ["user@host"]

        it "preserves a comma inside an identifier" $
            "(a,b)" `sExpressionShouldBe` ["a,b"]

        it "preserves comma-at at the start of an identifier" $
            "(,@x)" `sExpressionShouldBe` [",@x"]

        it "keeps detached comma-at as a lexeme" $
            "(,@ (xs))" `sExpressionShouldBe` [",@", "xs"]

        it "rejects a bare top-level comma-at identifier" $
            parseMaybe sExpression ",@x" `shouldBe` Nothing

        it "successfully lex a basic list" $ do
            "(a b c)" `sExpressionShouldBe` ["a", "b", "c"]

        it "successfully parse nil inside parentheses" $ do
            "(nil)" `sExpressionShouldBe` ["nil"]

        it "fail to parse a standalone nil" $ do
            let mSExpr = parseMaybe sExpression "nil"
            mSExpr `shouldSatisfy` isNothing

        it "successfully lex a basic list with number" $ do
            parseTest sExpression "(1 a)"
            "(1 a)" `sExpressionShouldBe` ["1", "a"]

        it "successfully lex a basic list with number and starting character" $ do
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
            "(() [] {})" `sExpressionShouldBe` []

        it "successfully lex internally nested s-expressions of each bracket" $ do
            "([{}])" `sExpressionShouldBe` []

        it "successfully lex a nested s-expressions" $ do
            "((()) (()))" `sExpressionShouldBe` []

        it "successfully lex a nested s-expressions of different brackets" $ do
            "({()} [{}])" `sExpressionShouldBe` []

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

        it "associates inner '#' with the s-expression" $ do
            let Just [result] = parseMaybe snailAst "(different-char/2 #(inside))"
            foldLexemes result `shouldBe` ["different-char/2", "inside"]

        it "keeps a run of prefix characters on the inner list" $
            case parseMaybe snailAst "(@,(xs))" of
                Just [SExpression Nothing Round [SExpression (Just "@,") Round [Lexeme (_, "xs")]]] -> pure ()
                result -> expectationFailure $ show result

        it "round-trips a list whose only child has the at-comma prefix" $
            case parseMaybe sExpression "(@,(xs))" of
                Just ast@(SExpression Nothing Round [SExpression (Just "@,") Round [Lexeme (_, "xs")]]) ->
                    toText ast `shouldBe` "(@,(xs))"
                result -> expectationFailure $ "Expected an @,-prefixed inner list, got " <> show result

        it "leaves a space-separated prefix token as a sibling of the list" $
            case parseMaybe snailAst "(@, (xs))" of
                Just [SExpression Nothing Round [Lexeme (_, "@,"), SExpression Nothing Round [Lexeme (_, "xs")]]] -> pure ()
                result -> expectationFailure $ show result

        it "preserves mixed and repeated prefix characters on a top-level list" $
            case parseMaybe snailAst "`''#@,[xs]" of
                Just [SExpression (Just "`''#@,") Square [Lexeme (_, "xs")]] -> pure ()
                result -> expectationFailure $ show result
