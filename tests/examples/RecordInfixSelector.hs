data RecordWithInfixSelector = Cons { (<>) :: Int -> Int }

idRecord = Cons { (<>) = id }
