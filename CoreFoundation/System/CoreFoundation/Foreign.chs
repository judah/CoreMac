module System.CoreFoundation.Foreign(
                -- * Foreign interaction with 'CFTypeRef's
                -- $foreign
                -- Marshalling Core Foundation objects to/from Haskell
                CFType,
                CFTypeRef,
                withObject,
                withDynObject,
                withVoidObject,
                withMaybeObject,
                withObjects,
                unsafeObjectToPtr,
                touch,
                withMutableObject,
                getOwned,
                getAndRetain,
                maybeGetOwned,
                maybeGetAndRetain,
                retainCFTypeRef,
                ensureTrue,
                -- * Mutable types
                unsafeFreeze,
                unsafeThaw,
                -- *  Allocators
                AllocatorRef,
                withDefaultAllocator,

                ) where

import System.CoreFoundation.Internal.Unsafe

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad (when)
import Control.Monad.Primitive (touch)
import Control.Exception (Exception, throw, bracketOnError)

#include <CoreFoundation/CoreFoundation.h>

----------
-- Foreign retain/release functions

-- Retains a Core Foundation object.  Returns the input.
-- If NULL, causes a crash.
foreign import ccall "CFRetain" cfRetain :: Ptr a -> IO (Ptr a)

-- Releases a Core Foundation object which must not be NULL.
foreign import ccall "&CFRelease" cfReleasePtr :: FunPtr (Ptr a -> IO ())

-- | On a NULL pointer, does nothing. Otherwise, calls 'cfRelease'.
checkedRelease :: Ptr a -> IO ()
checkedRelease ptr
  | ptr == nullPtr = return ()
  | otherwise = cfRelease ptr

-- | On a NULL pointer, returns NULL. Otherwise, calls 'cfRetain'.
checkedRetain :: Ptr a -> IO (Ptr a)
checkedRetain ptr
  | ptr == nullPtr = return ptr
  | otherwise = cfRetain ptr
------------------------------


-- Helper
{#fun CFGetTypeID as cfTypeID
    { castPtr `Ptr a' } -> `TypeID' TypeID #}

{- $foreign

We note one caveat about the foreign export functions.   Namely, the pure
object constructors like @String.fromChars@ and @Number.newNumber@ break
referential transparency if the underlying 'CFTypeRef's are tested for equality.  For
example:

> -- Returns False
> test1 = let
>           str1 = fromChars "foo"
>           str2 = fromChars "foo"
>         in withObject str1 $ \p1 -> withObject str2 $ \p2 -> return p1==p2
> 
> -- Returns True
> test1 = let
>           str = fromChars "foo"
>         in withObject str $ \p1 -> withObject str $ \p2 -> return p1==p2

In general, however, this should not cause problems when using the 
Core Foundation API functions.

-}

-- | Like 'withForeignPtr', extracts the underlying C type and keeps the object alive
-- while the given action is running.
-- It is not safe in general to use the 'CFTypeRef' after the action completes.
withObject :: Object a => a -> (Ptr (Repr a) -> IO b) -> IO b
withObject = withForeignPtr . unsafeUnObject

-- | 'withObject', but with a pointer cast
withDynObject :: Object a => a -> (Ptr CFType -> IO b) -> IO b
withDynObject a k = withObject a (k . castPtr)

-- | Like 'withObject', except that if the input is Nothing, the action will be passed a 'nullPtr'.
withMaybeObject :: Object a => Maybe a -> (Ptr (Repr a) -> IO b) -> IO b
withMaybeObject Nothing = ($ nullPtr)
withMaybeObject (Just o) = withObject o

-- | Like 'withObject', but casts the ptr to a @void*@
withVoidObject :: Object a => a -> (Ptr () -> IO b) -> IO b
withVoidObject obj k = withObject obj (k . castPtr)

-- TODO: is this inefficient?
withObjects :: Object a => [a] -> ([Ptr (Repr a)] -> IO b) -> IO b
withObjects [] act = act []
withObjects (o:os) act = withObject o $ \p -> withObjects os $ \ps -> act (p:ps)

-- | Extract the underlying pointer. Make sure to 'touch' the object after using the 'Ptr',
-- to make sure that the object isn't finalised while the 'Ptr' is still alive.
--
-- Prefer using 'withObject' if possible, which automates the 'touch'ing.
unsafeObjectToPtr :: Object a => a -> Ptr (Repr a)
unsafeObjectToPtr = unsafeForeignPtrToPtr . unsafeUnObject

-- | Like 'withObject', except that we use the passed-in *mutable* object as an *immutable* one
withMutableObject :: Object a => Mutable s a -> (Ptr (Repr a) -> IO b) -> IO b
withMutableObject = withObject . unsafeUnMutable

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you own, for example those obtained 
-- from a @Create@ or @Copy@ function.  
-- 
-- At some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getOwned :: forall a . Object a => IO (Ptr (Repr a)) -> IO a
getOwned gen = do
  res <- maybeGetOwned gen
  case res of
    Nothing -> fail "System.CoreFoundation.Foreign.getOwned: unexpected NULL object"
    Just p -> return p

-- | Retuns a Haskell type which references the given Core Foundation C object.
-- This function performs the same as 'getOwned', except that it returns 'Nothing'
-- if the input is NULL.
maybeGetOwned :: Object a => IO (Ptr (Repr a)) -> IO (Maybe a)
maybeGetOwned gen =
  bracketOnError
    gen
    checkedRelease
    (\ptr ->
        if ptr == nullPtr
          then return Nothing
          else (Just . unsafeObject) `fmap` newForeignPtr cfReleasePtr ptr)

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you do not own, for example 
-- those obtained from a @Get@ function.  
-- 
-- This function calls @CFRetain@ on its argument.  At
-- some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getAndRetain :: forall a . Object a => Ptr (Repr a) -> IO a
getAndRetain p = do
    checkCFTypeRef "getAndRetain" p $ maybeStaticTypeID (undefined :: a)
    cfRetain p >>= fmap unsafeObject . newForeignPtr cfReleasePtr

-- | Retuns a Haskell type which references the given Core Foundation C object.
-- This function performs the same as 'getAndRetain', except that it returns 'Nothing'
-- if the input is NULL.
maybeGetAndRetain :: Object a => Ptr (Repr a) -> IO (Maybe a)
maybeGetAndRetain p
    | p==nullPtr = return Nothing
    | otherwise = fmap Just $ getAndRetain p

-- | Checks that the given pointer is non-null and of the right type.
-- If not, throws an error.
checkCFTypeRef :: String -> Ptr a -> Maybe TypeID -> IO ()
checkCFTypeRef descr p maybeStaticID
    | p==nullPtr = error $ descr ++ ": null object"
    | otherwise = case maybeStaticID of
                    Nothing -> return ()
                    Just staticID -> do
                        dynTypeID <- cfTypeID p
                        when (dynTypeID /= staticID)
                            $ error $ descr ++ ": type mismatch; "
                                ++ "expected " ++ show (typeIDDescription staticID)
                                ++ ", got " ++ show (typeIDDescription dynTypeID)

{- | Returns the underlying C object, after calling an extra @CFRetain@ on it.

The C object will not be deallocated until some point after @CFRelease@ has been
called on it by foreign code (C or Objective-C).  It will also stay alive while the
Haskell type is in scope.  Every call to
'retainCFTypeRef' must be matched by exactly one call to 'CFRelease'.

Objective-C code has a few alternatives to calling 'CFRelease'; however, 
they depend on the type of memory management used by the foreign code:

  - When using manual memory management, if the 'CFTypeRef' is toll-free bridged to an
    Objective-C type then you may call @[obj autorelease]@ or @[obj release]@ instead of
    'CFRelease'.

  - When using garbage collection, you may use either 'CFMakeCollectable' or 'CFRelease'.
    You should not use @[obj release]@ or @[obj autorelease]@, as those are no-ops when
    using GC.
    
  - When using Automatic Reference Counting, if the 'CFTypeRef' is toll-free bridged 
    you may use @CFBridgingRelease@ instead of @CFRelease@ to indicate that ARC will be
    responsible for releasing the object. (Untested.)
-}
retainCFTypeRef :: Object a => a -> IO (Ptr (Repr a))
retainCFTypeRef x = withObject x checkedRetain

{- |
If the boolean is true (nonzero), returns @()@, otherwise throws the exception.
-}
ensureTrue :: Exception e => e -> CBoolean -> IO ()
ensureTrue exc b = if b == 0 then throw exc else return ()
--------



----------

type AllocatorRef = Ptr ()

withDefaultAllocator :: (AllocatorRef -> IO a) -> IO a
withDefaultAllocator f = f nullPtr



