import Test.QuickCheck
import qualified Foreign.C.Math.Double as C

main = sequence_
    [ quickCheck $ \x       -> C.acos x     == acos x
    , quickCheck $ \x       -> C.asin x     == asin x
    , quickCheck $ \x       -> C.atan x     == atan x

    -- note:
    , quickCheck $ \x y     -> C.atan2 x y  == atan2 x y

    , quickCheck $ \x       -> C.cos x      == cos x
    , quickCheck $ \x       -> C.sin x      == sin x
    , quickCheck $ \x       -> C.tan x      == tan x
    , quickCheck $ \x       -> C.cosh x      == cosh x
    , quickCheck $ \x       -> C.sinh x      == sinh x
    , quickCheck $ \x       -> C.tanh x      == tanh x
    , quickCheck $ \x       -> C.exp  x      == exp  x

    , quickCheck $ \x       -> C.log  x      == log  x

    , quickCheck $ \x y     -> x >= 0 ==> C.pow  x y    ==  x ** y
    , quickCheck $ \x      -> x >= 0 ==> C.sqrt x == sqrt x

    ]
