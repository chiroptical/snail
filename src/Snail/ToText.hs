module Snail.ToText (toText) where

import Data.Text
import Snail.Lexer

toText :: SnailAst -> Text
toText = \case
    TextLiteral (_, txt) -> "\"" <> txt <> "\""
    Lexeme (_, lexeme) -> lexeme
    SExpression Nothing exprs ->
        let txt = Data.Text.unwords $ toText <$> exprs
         in "(" <> txt <> ")"
    SExpression (Just c) exprs ->
        let txt = Data.Text.unwords $ toText <$> exprs
         in singleton c <> "(" <> txt <> ")"
