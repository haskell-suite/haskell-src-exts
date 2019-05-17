{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

import THTypedSplices.Defs

main = $$justTH ($$([e|| 2 ||]))
