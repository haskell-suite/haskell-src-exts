{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
import Language.Haskell.TH

data PageFunction m a = PF

main = let a = mkName "a" in
       runQ [t| forall m. PageFunction m $(conT (mkName "a")) |] >>= print

