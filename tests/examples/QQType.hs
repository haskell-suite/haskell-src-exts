{-# LANGUAGE FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module QQType where

import Language.Haskell.TH

x :: DecsQ
x = [d| instance Show $(conT (mkName \"Int\")) |]
