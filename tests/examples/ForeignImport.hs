{- If compiled without ForeignFunctionInterface (part of Haskell2010),
   it complains not about FFI but about missing TemplateHaskell -}
foreign import ccall unsafe "getProgArgv"
 getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
