{-# LANGUAGE UnicodeSyntax #-}

arrowTest ∷ String →   String
arrowTest input =
    case input of
      "hello" → "world"
      "world" → "hello"
      otherwise → "unknow"
