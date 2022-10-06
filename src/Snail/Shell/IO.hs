module Snail.Shell.IO where

import Data.Text.IO qualified as Text
import Snail.Shell.Lexer
import Text.Megaparsec

-- | Given a 'FilePath', attempt to parse 'SExpression's from that file.
readSnailFile :: FilePath -> IO (Either String [SExpression])
readSnailFile fp = do
    contents <- Text.readFile fp
    pure $ case parse sExpressions (show fp) contents of
        Left parseErrorBundle -> Left $ errorBundlePretty parseErrorBundle
        Right sexprs -> Right sexprs
