module Snail.IO where

import Data.Text.IO qualified as Text
import Snail.Lexer
import Text.Megaparsec

-- | Given a 'FilePath', attempt to parse 'SnailAst' from a file.
readSnailFile :: FilePath -> IO (Either String [SnailAst])
readSnailFile fp = do
    contents <- Text.readFile fp
    pure $ case parse snailAst (show fp) contents of
        Left parseErrorBundle -> Left $ errorBundlePretty parseErrorBundle
        Right sexprs -> Right sexprs
