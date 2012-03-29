module System.CoreFoundation.Base(
                -- * Core Foundation Objects
                Object(..),
                touchObject,
                getObjectDescription,
                hash,
                equal,
                -- * TypeIDs
                TypeID(),
                typeIDDescription,
                StaticTypeID(..),
                staticTypeID,
                dynamicTypeID,
                staticTypeDescription,
                dynamicTypeDescription,
                -- * Dynamic types
                DynObj,
                dyn,
                castObject,
                castObjectOrError,
                -- * Mutable objects
                Mutable(),
                unsafeFreeze,
                unsafeThaw,
                MutableRepr,
                -- *  Miscellaneous type synonyms
                CBoolean,
                CFIndex,
                cvtEnum,
                ) where

import Foreign.Marshal (toBool)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable
import Control.DeepSeq


import System.CoreFoundation.Internal.Unsafe
import System.CoreFoundation.Foreign

#include <CoreFoundation/CoreFoundation.h>
-- repeated from System.CoreFoundation.Internal.Unsafe to avoid c2hs dependency errors.
-- That is, we would prefer to tell c2hs to {#import <...>.Unsafe#}, but since the Unsafe
-- module is a cabal "Other-Module", c2hs will be run on it *after* it is run on Base.
{#pointer CFTypeRef nocode#}

-- | Like 'touchForeignPtr', ensures that the object will still be alive
-- at the given place in the sequence of IO events.
touchObject :: Object a => a -> IO ()
touchObject = touchForeignPtr . unsafeUnObject

-- | Examines the given 'CFTypeRef' to determine its type.
{#fun pure CFGetTypeID as dynamicTypeID
    `Object a' => { withDynObject* `a' } -> `TypeID' TypeID #}

-- | Returns the 'TypeID' associated with objects of type @a@.  
-- Does not use its argument.
staticTypeID :: StaticTypeID a => a -> TypeID
staticTypeID = unsafeStaticTypeID




cvtEnum :: (Enum a, Enum b) => a -> b
cvtEnum = toEnum . fromEnum

-------
-- Misc types

------
-- Getting the String from a TypeID.
-- The CF.String module provides a better API, but using it would lead to cyclic imports.

-- | Returns a textual description of the Core Foundation type associated with the Haskell type @a@.
staticTypeDescription :: StaticTypeID a => a -> String
staticTypeDescription = typeIDDescription . staticTypeID

-- | Returns a textual description of the type of the given Core Foundation object.
dynamicTypeDescription :: Object a => a -> String
dynamicTypeDescription = typeIDDescription . dynamicTypeID

-- | A 'DynObj' wraps a Core Foundation object of unknown type.
newtype DynObj = DynObj (ForeignPtr CFType)
  deriving Typeable

instance NFData DynObj

instance Object DynObj where
    type Repr DynObj = CFType
    unsafeObject = DynObj
    unsafeUnObject (DynObj o) = o
    maybeStaticTypeID _ = Nothing

dyn :: Object a => a -> DynObj
dyn = DynObj . castForeignPtr . unsafeUnObject

castObject  :: forall a . Object a => DynObj -> Maybe a
castObject o@(DynObj p) = case maybeStaticTypeID (undefined :: a) of
                    Just t | t /= dynamicTypeID o   -> Nothing
                    _                               -> Just $ unsafeObject (castForeignPtr p)

{#fun CFCopyDescription as getObjectDescription
    `Object a' => { withDynObject* `a' } -> `String' peekCFStringRef* #}

-- | Throws an error if the input is not of the given type.
castObjectOrError :: forall a . Object a => DynObj -> a
castObjectOrError o@(DynObj p)
    = case maybeStaticTypeID (undefined :: a) of
        Just t | t /= dynamicTypeID o   -> error $ "unsafeCastObject: expected type "
                                            ++ show (typeIDDescription t)
        _ -> unsafeObject (castForeignPtr p)


-------------------- Determining equality
{- |
Determines whether two Core Foundation objects are considered equal.

 [Discussion] Equality is something specific to each Core Foundation opaque type. For example, two CFNumber objects are equal if the numeric values they represent are equal. Two CFString objects are equal if they 
represent identical sequences of characters, regardless of encoding.
-}
{#fun pure unsafe CFEqual as equal
  `Object a' => { withDynObject* `a', withDynObject* `a' } -> `Bool' #}

--------- Hashing
-- | type used for CoreFoundation hashes. Use 'fromIntegral' for conversions as necessary.
newtype HashCode = HashCode {#type CFHashCode#}
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

{- |
Returns a code that can be used to identify an object in a hashing structure.

 [Discussion] Two objects that are equal (as determined by the 'equal' function) have the same hashing value. However, the converse is not true: two objects with the same hashing value might not be equal. That is, hashing values are not necessarily unique. The hashing value for an object might change from release to release or from platform to platform.
-}
{#fun pure unsafe CFHash as hash
  `Object a' => { withDynObject* `a' } -> `HashCode' HashCode #}
