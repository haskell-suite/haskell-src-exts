{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
main = ($$([|| Just ||]) :: a -> Maybe a) ($$([e|| 2 ||]) :: Int)
