module Gen where

import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck

-- | TODO: Should define the list of characters allowed in symbols in Snail somewhere
genValidSymbolChar :: Gen Char
genValidSymbolChar = elements ['a' .. 'z']

genSymbol :: Gen Text
genSymbol = T.pack <$> listOf genValidSymbolChar
