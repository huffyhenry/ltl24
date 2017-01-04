module Spec where

import LTL24

data Spec = Spec {
    specName :: String,
    specFormula :: LTL24
}

instance Eq Spec where
    s1 == s2 = (specName s1) == (specName s2)
