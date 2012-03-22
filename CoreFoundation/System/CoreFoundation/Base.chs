module System.CoreFoundation.Base(
                -- * Core Foundation Objects
                Object(),
                touchObject,
                withDynObject,
                -- * TypeIDs
                TypeID(),
                typeIDDescription,
                StaticTypeID,
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
                unMutable,
                -- *  Miscellaneous type synonyms
                CBoolean,
                CFIndex,
                cvtEnum,
                ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import System.CoreFoundation.Internal.Unsafe
import System.CoreFoundation.Foreign

#include <CoreFoundation/CoreFoundation.h>


-- | Like 'touchForeignPtr', ensures that the object will still be alive
-- at the given place in the sequence of IO events.
touchObject :: Object a => a -> IO ()
touchObject = touchForeignPtr . unsafeUnObject

withDynObject :: Object a => a -> (Ptr CFType -> IO b) -> IO b
withDynObject a k = withObject a (k . castPtr)

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


