{-# LANGUAGE PatternSynonyms #-}


pattern Syn :: Typ

pattern Syn :: () => () => Typ

pattern Syn :: Show a => Show b => Typ

pattern Syn :: Show b => Typ

pattern Syn :: Show b => () => Typ

pattern Syn :: () => Show b => Typ

