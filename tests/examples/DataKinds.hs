{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

class C1 (a :: Bool {- 1 -}) where
    c :: proxy a -> Int

class C2 (a :: [ * ])

data Baz = Baz
data Foo = Foo

class C3 (a :: [(Baz, Foo)])

class C4 (a :: ( * ))


class C5 (a :: App foo baz)

class C6 (a :: (parens))

data X (a :: [*])

x1 = undefined :: X '[Int]
x2 = undefined :: X '[Int, Double]

data Y (a :: (*, Bool))

y1 = undefined :: Y '(Double, True)
y2 = undefined :: Y '(Double, 'False {-comment-})


z1 = undefined :: X [ a -> b, X '[] ]
z2 = undefined :: Y (a -> b, True)
