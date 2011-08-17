-- | 'Number' is a class which wraps basic C scalar (numeric) types.
-- It is toll-free bridged with 'NSNumber'.
module System.CoreFoundation.Number(
                Number,
                NumberRef,
                newNumber,
                numberValue,
                numberType,
                IsNumberType,
                NumberType(..),
                isFloatType,
                ) where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Number"

-- Since Number doesn't have a mutable analogue, it's OK
-- for the accessor functions to be mutable.

-- | Returns whether the 'Number' contains a value stored internally
-- as one of the floating point types.
{#fun CFNumberIsFloatType as isFloatType
    { withCF* `Number' } -> `Bool' '(>0)' #}
    
{#enum define NumberType
    { kCFNumberSInt8Type as Int8Type
    , kCFNumberSInt16Type as Int16Type
    , kCFNumberSInt32Type as Int32Type
    , kCFNumberSInt64Type as Int64Type
    , kCFNumberFloat32Type as Float32Type
    , kCFNumberFloat64Type as Float64Type
    , kCFNumberCharType as CCharType
    , kCFNumberShortType as CShortType
    , kCFNumberIntType as CIntType
    , kCFNumberLongType as CLongType
    , kCFNumberLongLongType as CLLongType
    , kCFNumberFloatType as CFloatType
    , kCFNumberDoubleType as CDoubleType
    , kCFNumberCFIndexType as CFIndexType
    , kCFNumberNSIntegerType as NSIntegerType
    , kCFNumberCGFloatType as CGFloatType
    } deriving (Show,Eq) #}

-- Returns the type used by the 'Number' object to store its value. 
-- 
-- The type specified by 'newNumber' is not necessarily preserved when 
-- a new 'Number' is created --- it uses whatever internal storage type
-- it deems appropriate.
{#fun pure unsafe CFNumberGetType as numberType
    { withCF* `Number' } -> `NumberType' cvtEnum #}

class Storable a => IsNumberType a where
    numberTypeOf :: a -> NumberType

instance IsNumberType Int8 where
    numberTypeOf _ = Int8Type

instance IsNumberType Int16 where
    numberTypeOf _ = Int16Type

instance IsNumberType Int32 where
    numberTypeOf _ = Int32Type

instance IsNumberType CChar where
    numberTypeOf _ = CCharType

instance IsNumberType CShort where
    numberTypeOf _ = CShortType

instance IsNumberType CInt where
    numberTypeOf _ = CIntType

instance IsNumberType CLong where
    numberTypeOf _ = CLongType

instance IsNumberType CLLong where
    numberTypeOf _ = CLLongType

instance IsNumberType CFloat where
    numberTypeOf _ = CFloatType

instance IsNumberType CDouble where
    numberTypeOf _ = CDoubleType

instance IsNumberType (Ptr a) where
    numberTypeOf _ = case sizeOf nullPtr of
                                4 -> Int32Type
                                8 -> Int64Type
                                _ -> error "Unknown size of Ptr"

instance IsNumberType Int where
    numberTypeOf _ = case sizeOf (0 :: Int) of
                            4 -> Int32Type
                            8 -> Int64Type
                            _ -> error "Unknown size of Int"

{#fun unsafe CFNumberGetValue as getNumberValue
    { withCF* `Number'
    , cvtEnum `NumberType'
    , castPtr `Ptr a'
    } -> `CBoolean' id #}

-- TODO: error checking.  (Currently it will do lossy conversion.)

-- | Gets the value in the Number, cast to a specific type.
numberValue :: forall a . IsNumberType a => Number -> a
numberValue n = unsafePerformIO $ alloca $ \p -> do
                getNumberValue n (numberTypeOf (undefined :: a)) p
                peek p

{#fun unsafe CFNumberCreate as numberCreate
    { withDefaultAllocator- `AllocatorPtr'
    , cvtEnum `NumberType'
    , castPtr `Ptr a'
    } -> `Number' getOwned* #}

newNumber :: forall a . IsNumberType a => a -> IO Number
newNumber n = with n $
                numberCreate (numberTypeOf (undefined :: a))
