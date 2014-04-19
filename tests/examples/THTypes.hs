{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module THTypes where

import Language.Haskell.TH

x :: DecsQ
x = [d| instance Show $(conT (mkName \"Int\")) |]

unit x = [t| $x |]
