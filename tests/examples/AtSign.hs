{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{- |
Accelerate interface to the native CUDA implementation
of the Fourier Transform provided by the CUFFT library.
-}
module Data.Array.Accelerate.CUFFT.Private where


transform hndl@(Handle fallback mode width _) =
   wrap mode (A.constant width) $
   A.foreignAcc
      (AF.CUDAForeignAcc "transformForeign" $ transformForeign hndl)
      (unwrap mode (A.constant width) fallback)


