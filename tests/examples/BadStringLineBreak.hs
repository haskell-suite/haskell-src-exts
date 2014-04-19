module BadStringLineBreak where

main = print $ "hello" ++ "world
-- any random junk that goes here gets added onto the character count
-- and the quote ends it with some garbage "
