{-# LANGUAGE CPP, ForeignFunctionInterface #-}

--------------------------------------------------------------------
-- |
-- Module    : Foreign.C.Math.Double
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability: FFI
--
--------------------------------------------------------------------
--
-- A binding to C's math.h double functions.
--

module Foreign.C.Math.Double where

import Prelude (Double,realToFrac,fromIntegral,($),return,IO)

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc

#include <math.h>


-- | The acos function computes the principal value of the arc cosine of x
-- in the range [0, pi]
--
acos :: Double -> Double
acos x = realToFrac (c_acos (realToFrac x))
{-# INLINE acos #-}

foreign import ccall unsafe "math.h acos"
     c_acos     :: CDouble -> CDouble

-- | The asin function computes the principal value of the arc sine of x in
-- the range [-pi/2, +pi/2].
--
asin :: Double -> Double
asin x = realToFrac (c_asin (realToFrac x))
{-# INLINE asin #-}

foreign import ccall unsafe "math.h asin"
     c_asin     :: CDouble -> CDouble

-- | The atan function computes the principal value of the arc tangent of x
-- in the range [-pi/2, +pi/2].
--
atan :: Double -> Double
atan x = realToFrac (c_atan (realToFrac x))
{-# INLINE atan #-}

foreign import ccall unsafe "math.h atan"
     c_atan     :: CDouble -> CDouble

-- | The atan2 function computes the principal value of the arc tangent of
-- y/x, using the signs of both arguments to determine the quadrant of the
-- return value.
--
atan2 :: Double -> Double -> Double
atan2 x y = realToFrac (c_atan2 (realToFrac x) (realToFrac y))
{-# INLINE atan2 #-}

foreign import ccall unsafe "math.h atan2"
     c_atan2    :: CDouble -> CDouble -> CDouble

-- | The cos function computes the cosine of x (measured in radians).
-- A large magnitude argument may yield a result with little or no significance.  For a
-- discussion of error due to roundoff, see math(3).
--
cos :: Double -> Double
cos x = realToFrac (c_cos (realToFrac x))
{-# INLINE cos #-}

foreign import ccall unsafe "math.h cos"
     c_cos      :: CDouble -> CDouble

-- | The sin function computes the sine of x (measured in radians). 
-- A large magnitude argument may yield a result with little or no
-- significance.  For a discussion of error due to roundoff, see math(3).
--
sin :: Double -> Double
sin x = realToFrac (c_sin (realToFrac x))
{-# INLINE sin #-}

foreign import ccall unsafe "math.h sin"
     c_sin      :: CDouble -> CDouble

-- | The tan function computes the tangent of x (measured in radians). 
-- A large magnitude argument may yield a result with little or no
-- significance.  For a discussion of error due to roundoff, see math(3).
--
tan :: Double -> Double
tan x = realToFrac (c_tan (realToFrac x))
{-# INLINE tan #-}

foreign import ccall unsafe "math.h tan"
     c_tan      :: CDouble -> CDouble

-- | The cosh function computes the hyperbolic cosine of x.
--
cosh :: Double -> Double
cosh x = realToFrac (c_cosh (realToFrac x))
{-# INLINE cosh #-}

foreign import ccall unsafe "math.h cosh"
     c_cosh     :: CDouble -> CDouble

-- | The sinh function computes the hyperbolic sine of x.
--
sinh :: Double -> Double
sinh x = realToFrac (c_sinh (realToFrac x))
{-# INLINE sinh #-}

foreign import ccall unsafe "math.h sinh"
     c_sinh     :: CDouble -> CDouble

-- | The tanh function computes the hyperbolic tangent of x.
--
tanh :: Double -> Double
tanh x = realToFrac (c_tanh (realToFrac x))
{-# INLINE tanh #-}

foreign import ccall unsafe "math.h tanh"
     c_tanh     :: CDouble -> CDouble

------------------------------------------------------------------------

-- | The exp() function computes the exponential value of the given argument x. 
--
exp :: Double -> Double
exp x = realToFrac (c_exp (realToFrac x))
{-# INLINE exp  #-}

foreign import ccall unsafe "math.h exp"
     c_exp      :: CDouble -> CDouble

-- | frexp convert floating-point number to fractional and integral components
-- frexp is not defined in the Haskell 98 report.
--
frexp :: Double -> (Double,Int)
frexp x = unsafePerformIO $
    alloca $ \p -> do
        d <- c_frexp (realToFrac x) p
        i <- peek p
        return (realToFrac d, fromIntegral i)

foreign import ccall unsafe "math.h frexp"
     c_frexp    :: CDouble -> Ptr CInt -> IO Double

-- | The ldexp function multiplies a floating-point number by an integral power of 2.
-- ldexp is not defined in the Haskell 98 report.
--
ldexp :: Double -> Int -> Double
ldexp x i = realToFrac (c_ldexp (realToFrac x) (fromIntegral i))
{-# INLINE ldexp #-}

foreign import ccall unsafe "math.h ldexp"
     c_ldexp    :: CDouble -> CInt -> Double

-- | The log() function computes the value of the natural logarithm of argument x.
--
log :: Double -> Double
log x = realToFrac (c_log (realToFrac x))
{-# INLINE log  #-}

foreign import ccall unsafe "math.h log"
     c_log      :: CDouble -> CDouble

-- | The log10 function computes the value of the logarithm of argument x to base 10.
-- log10 is not defined in the Haskell 98 report.
log10 :: Double -> Double
log10 x = realToFrac (c_log10 (realToFrac x))
{-# INLINE log10 #-}

foreign import ccall unsafe "math.h log10"
     c_log10    :: CDouble -> CDouble

-- | The modf function breaks the argument value into integral and fractional
-- parts, each of which has the same sign as the argument.
-- modf is not defined in the Haskell 98 report.
--
modf :: Double -> (Double,Double)
modf x = unsafePerformIO $
    alloca $ \p -> do
        d <- c_modf (realToFrac x) p
        i <- peek p
        return (realToFrac d, realToFrac i)

foreign import ccall unsafe "math.h modf"
     c_modf     :: CDouble -> Ptr CDouble -> IO CDouble

-- | The pow function computes the value of x to the exponent y.
--
pow :: Double -> Double -> Double
pow x y = realToFrac (c_pow (realToFrac x) (realToFrac y))
{-# INLINE pow #-}

foreign import ccall unsafe "math.h pow"
     c_pow      :: CDouble -> CDouble -> CDouble

-- | The sqrt function computes the non-negative square root of x.
--
sqrt :: Double -> Double
sqrt x = realToFrac (c_sqrt (realToFrac x))
{-# INLINE sqrt #-}

foreign import ccall unsafe "math.h sqrt"
     c_sqrt     :: CDouble -> CDouble

-- | The ceil function returns the smallest integral value greater than or equal to x.
--
ceil :: Double -> Double
ceil x = realToFrac (c_ceil (realToFrac x))
{-# INLINE ceil #-}

foreign import ccall unsafe "math.h ceil"
     c_ceil     :: CDouble -> CDouble

-- | The fabs function computes the absolute value of a floating-point number x.
--
fabs :: Double -> Double
fabs x = realToFrac (c_fabs (realToFrac x))
{-# INLINE fabs #-}

foreign import ccall unsafe "math.h fabs"
     c_fabs     :: CDouble -> CDouble

-- | The floor function returns the largest integral value less than or equal to x.
--
floor :: Double -> Double
floor x = realToFrac (c_floor (realToFrac x))
{-# INLINE floor #-}

foreign import ccall unsafe "math.h floor"
     c_floor    :: CDouble -> CDouble

-- | The fmod function computes the floating-point remainder of x \/ y.
--
fmod :: Double -> Double -> Double
fmod x y = realToFrac (c_fmod (realToFrac x) (realToFrac y))
{-# INLINE fmod #-}

foreign import ccall unsafe "math.h fmod"
     c_fmod     :: CDouble -> CDouble -> CDouble

-- | The round function returns the nearest integral value to x; if x lies
-- halfway between two integral values, then these functions return the integral
-- value with the larger absolute value (i.e., it rounds away from zero).
-- 
round :: Double -> Double
round x = realToFrac (c_round (realToFrac x))
{-# INLINE round #-}

foreign import ccall unsafe "math.h round"
     c_round    :: CDouble -> CDouble

-- | The fmod function computes the floating-point remainder of x \/ y.
--
trunc :: Double -> Double
trunc x = realToFrac (c_trunc (realToFrac x))
{-# INLINE trunc #-}

foreign import ccall unsafe "math.h trunc"
     c_trunc    :: CDouble -> CDouble
