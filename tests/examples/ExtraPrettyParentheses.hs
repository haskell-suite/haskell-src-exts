data Q a = Q a a

-- In this example, the pretty-printer should insert extra parentheses around
-- the negative literal pattern.
x * - 1 = negate x

-- In these examples, the pretty-printer should not insert extra parentheses.
Just x <> Nothing = Just x
f (- 1 `Q` _) = ()
