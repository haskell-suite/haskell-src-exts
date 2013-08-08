{-# LANGUAGE MagicHash #-}
module HappyDoAction where

foo :: Int#

happyDoAction i tk st
    = {- nothing -}


      case action of
        0#        -> {- nothing -}
                     happyFail i tk st
        -1#       -> {- nothing -}
                     happyAccept i tk st
        n | (n <# (0# :: Int#)) -> {- nothing -}

                     (happyReduceArr ! rule) i tk st
                     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
        n         -> {- nothing -}


                     happyShift new_state i tk st
                     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off +# i)
         check  = if (off_i >=# (0# :: Int#))
                   then (indexShortOffAddr happyCheck off_i ==#  i)
                   else False
         action | check     = indexShortOffAddr happyTable off_i
                | otherwise = indexShortOffAddr happyDefActions st
