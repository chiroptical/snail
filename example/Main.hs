module Main where

import Snail
import Control.Monad.Except
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

data Operation = Addition | Multiplication 
  deriving (Eq, Show)

data Ast
  = AstInt Integer
  | AstOperation Operation Ast Ast
  deriving (Eq, Show)

data LangError 
  = UnknownLexeme Text
  | UnknownOperation Text
  | TextLiteralUnsupported
  | EmptyExpression
  deriving (Eq, Show)

parseLeaf :: (MonadError LangError m) => Text -> m Ast
parseLeaf = \case
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
      Just int -> pure $ AstInt int

parseOp :: (MonadError LangError m) => Text -> m Operation
parseOp = \case
  "+" -> pure Addition
  "*" -> pure Multiplication
  txt -> throwError $ UnknownOperation txt

fromSnail :: (MonadError LangError m) => SnailAst -> m Ast
fromSnail = \case
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  SExpression _ _ [Lexeme (_, txt), argumentOne, argumentTwo] -> do
    op <- parseOp txt
    one <- fromSnail argumentOne
    two <- fromSnail argumentTwo
    pure $ AstOperation op one two
  -- empty expressions are invalid
  SExpression _ _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  SExpression leading fence exprs ->
    fromSnail . unwrap . SExpression leading fence $ unwrap <$> exprs

main :: IO ()
main = do
  case parseSnail "(+ 1 2)" of
    Left err -> print $ "Unable to parse snail: " <> show err
    Right [snail] -> runExceptT (fromSnail snail) >>= print
    Right _ -> putStrLn "Excepted single expression"
