{-# LANGUAGE RecordWildCards #-}
-- Should fail.
f A { .., b = v, .. } = v

