

data Bar = Bar

instance Eq Bar where
    {-# INLINE (==) #-}
    a == b = error "here"
