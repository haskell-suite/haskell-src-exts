{-# LANGUAGE GADTs #-}

data CmmNode e x where
        CmmEntry :: {-# UNPACK #-} !Int -> CmmNode e x

data UI = UI {-# UNPACK #-} !Int
