{-# LANGUAGE QuasiQuotes #-}

module Snail.ToTextSpec (spec) where

import Snail
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ

spec :: Spec
spec = do
    describe "toText" $ do
        it "handles empty s-expression" $
            toText (SExpression Nothing Round []) `shouldBe` "()"
        it "handles basic text literal" $
            toText (SExpression Nothing Round [TextLiteral (sourcePos, "hello world")])
                `shouldBe` [r|("hello world")|]
        it "handles basic lexeme" $
            toText
                ( SExpression
                    Nothing
                    Round
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
