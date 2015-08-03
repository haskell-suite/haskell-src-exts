{-# LANGUAGE JavascriptFFI #-}

foreign import javascript unsafe "somethingUseful_ = $1"
    js_set_somethingUseful :: JSFun a -> IO ()
