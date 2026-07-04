{-# LANGUAGE DataKinds #-}
module PromotedSpace where

-- Promoted list containing promoted constructor with quote
type PL1 = '[ 'True ]

-- Promoted list containing promoted list
type PL2 = '[ '[Int] ]

-- Promoted list containing promoted tuple
type PL3 = '[ '(Int, Bool) ]

-- Promoted tuple containing promoted constructor with quote
type PT1 = '( 'True, False )

-- Promoted tuple containing promoted list
type PT2 = '( '[Int], Bool )

-- Promoted tuple containing promoted tuple
type PT3 = '( '(Int, Bool), Char )
