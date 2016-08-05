module Playground where

import EDSL
import Text.PrettyPrint (Doc)
import Data.BitCode.LLVM.Pretty

-- reexport pretty (to make intero happy :-))
pp :: (Pretty a) => a -> Doc
pp = pretty
