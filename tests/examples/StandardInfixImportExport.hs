module StandardInfixImportExport ((:+:)(..)) where

import Data.Tree ((:-:)(..), (:-:)(Node, (:-:)))

data a :+: b = Inl a | Inr b
