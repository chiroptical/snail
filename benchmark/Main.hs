module Main (main) where

import Control.DeepSeq (rnf)
import Criterion.Main
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Paths_snail_bench (getDataFileName)
import Snail (SnailAst (..), snailAst)
import Text.Megaparsec (errorBundlePretty, parse)

-- Force the complete result without rendering it or requiring library instances.
forceAst :: SnailAst -> ()
forceAst (Lexeme token) = rnf token
forceAst (TextLiteral literal) = rnf literal
forceAst (SExpression prefix bracket children) =
    rnf prefix `seq` bracket `seq` forceForest children

forceForest :: [SnailAst] -> ()
forceForest = foldr (seq . forceAst) ()

parseInput :: Text -> ()
parseInput input = case parse snailAst "<benchmark>" input of
    Left err -> error (errorBundlePretty err)
    Right forest -> forceForest forest

main :: IO ()
main = defaultMain [bgroup "fixtures" (map fixture files), bgroup "synthetic" workloads]
  where
    files = ["learnxyz.cl", "fennel-reference.fnl", "r5rs_pitfalls.scm"]
    fixture name = inputBench name (getDataFileName ("fixtures/" ++ name) >>= Text.readFile)
    workloads =
        [ inputBench "atoms" $ pure ("(" <> Text.replicate 512 "identifier-123 " <> ")")
        , inputBench "strings" $ pure ("(" <> Text.replicate 128 "\"some text with a \\\"quoted\\\" word\" " <> ")")
        , inputBench "comments" $ pure (Text.replicate 128 "-- line\n(  a {- outer {- inner -} -} b )\n")
        , inputBench "nested-lists" $ pure (Text.replicate 128 "(" <> "x" <> Text.replicate 128 ")")
        , inputBench "large-program" $ pure (Text.concat (map definition [1 .. 2048 :: Int]))
        ]
    definition n =
        "(define (f"
            <> Text.pack (show n)
            <> " x) -- generated function\n"
            <> "  (if (ready? x) [emit \"ready\" x] (retry x)))\n"
    inputBench name load = env load $ \input -> bench name (whnf parseInput input)
