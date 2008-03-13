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

-- | The erf calculates the error function of x. The error function is defined as:
--
-- > erf(x) = 2/sqrt(pi)*integral from 0 to x of exp(-t*t) dt.
--
erf :: Double -> Double
erf x = realToFrac (c_erf (realToFrac x))
{-# INLINE erf #-}

foreign import ccall unsafe "math.h erf"
     c_erf      :: CDouble -> CDouble

-- | The erfc function calculates the complementary error function of x;
-- that is erfc() subtracts the result of the error function erf(x) from
-- 1.0.  This is useful, since for large x places disappear.
--
erfc :: Double -> Double
erfc x = realToFrac (c_erfc (realToFrac x))
{-# INLINE erfc #-}

foreign import ccall unsafe "math.h erfc"
     c_erfc     :: CDouble -> CDouble

-- | The gamma function.
--
gamma :: Double -> Double
gamma x = realToFrac (c_gamma (realToFrac x))
{-# INLINE gamma #-}

foreign import ccall unsafe "math.h gamma"
     c_gamma    :: CDouble -> CDouble

-- | The hypot function function computes the sqrt(x*x+y*y) in such a way that
-- underflow will not happen, and overflow occurs only if the final result
-- deserves it.  
-- 
-- > hypot(Infinity, v) = hypot(v, Infinity) = +Infinity for all v, including NaN.
--
hypot :: Double -> Double -> Double
hypot x y = realToFrac (c_hypot (realToFrac x) (realToFrac y))
{-# INLINE hypot #-}

foreign import ccall unsafe "math.h hypot"
     c_hypot    :: CDouble -> CDouble -> CDouble

-- | The isinf function returns 1 if the number n is Infinity, otherwise 0.
--
isinf :: Double -> Int
isinf x = fromIntegral (c_isinf (realToFrac x))
{-# INLINE isinf #-}

foreign import ccall unsafe "math.h isinf"
     c_isinf    :: CDouble -> CInt

-- | The isnan function returns 1 if the number n is ``not-a-number'',
-- otherwise 0.
--
isnan :: Double -> Int
isnan x = fromIntegral (c_isnan (realToFrac x))
{-# INLINE isnan #-}

foreign import ccall unsafe "math.h isnan"
     c_isnan    :: CDouble -> CInt

-- | finite returns the value 1 just when -Infinity < x < +Infinity; otherwise
-- a zero is returned (when |x| = Infinity or x is NaN.
--
finite :: Double -> Int
finite x = fromIntegral (c_finite (realToFrac x))
{-# INLINE finite #-}

foreign import ccall unsafe "math.h finite"
     c_finite    :: CDouble -> CInt

-- | The functions j0() and j1() compute the Bessel function of the
-- first kind of the order 0 and the order 1, respectively, for the real
-- value x
--
j0 :: Double -> Double
j0 x = realToFrac (c_j0 (realToFrac x))
{-# INLINE j0 #-}

foreign import ccall unsafe "math.h j0"
     c_j0    :: CDouble -> CDouble

-- | The functions j0() and j1() compute the Bessel function of the
-- first kind of the order 0 and the order 1, respectively, for the real
-- value x
--
j1 :: Double -> Double
j1 x = realToFrac (c_j1 (realToFrac x))
{-# INLINE j1 #-}

foreign import ccall unsafe "math.h j1"
     c_j1    :: CDouble -> CDouble

-- | The functions y0() and y1() compute the linearly independent Bessel
-- function of the second kind of the order 0 and the order 1,
-- respectively, for the positive integer value x (expressed as a double)
--
y0 :: Double -> Double
y0 x = realToFrac (c_y0 (realToFrac x))
{-# INLINE y0 #-}

foreign import ccall unsafe "math.h y0"
     c_y0    :: CDouble -> CDouble

-- | The functions y0() and y1() compute the linearly independent Bessel
-- function of the second kind of the order 0 and the order 1,
-- respectively, for the positive integer value x (expressed as a double)
--
y1 :: Double -> Double
y1 x = realToFrac (c_y1 (realToFrac x))
{-# INLINE y1 #-}

foreign import ccall unsafe "math.h y1"
     c_y1    :: CDouble -> CDouble

-- | yn() computes the Bessel function of the second kind for the
-- integer Bessel0 n for the positive integer value x (expressed as a
-- double).
--
yn :: Int -> Double -> Double
yn x y = realToFrac (c_yn (fromIntegral x) (realToFrac y))
{-# INLINE yn #-}

foreign import ccall unsafe "math.h yn"
     c_yn    :: CInt -> CDouble -> CDouble

-- | lgamma(x) returns ln|| (x)|.
--
lgamma :: Double -> Double
lgamma x = realToFrac (c_lgamma (realToFrac x))
{-# INLINE lgamma #-}

foreign import ccall unsafe "math.h lgamma"
     c_lgamma    :: CDouble -> CDouble
