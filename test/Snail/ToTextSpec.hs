{-# LANGUAGE QuasiQuotes #-}

module Snail.ToTextSpec (spec) where

import Snail.Shell
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ

spec :: Spec
spec = do
    describe "toText" $ do
        it "handles empty s-expression" $
            toText (SExpression Nothing []) `shouldBe` "()"
        it "handles basic text literal" $
            toText (SExpression Nothing [TextLiteral (sourcePos, "hello world")])
                `shouldBe` [r|("hello world")|]
        it "handles basic lexeme" $
            toText
                ( SExpression
                    Nothing
                    [ Lexeme (sourcePos, "hello")
                    , Lexeme (sourcePos, "world")
                    ]
                )
                `shouldBe` [r|(hello world)|]

sourcePos :: SourcePos
sourcePos =
    SourcePos
        { sourceName = "..."
        , sourceLine = mkPos 1
        , sourceColumn = mkPos 1
        }
