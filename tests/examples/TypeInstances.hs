{-# LANGUAGE TypeFamilies #-}

class MapType v where
  type Key v :: *
  type instance Key v = TileKey v

  type Source v :: *
