-- | CoreFoundation @CFBoolean@ type. See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFBooleanRef/Reference/reference.html#>
module System.CoreFoundation.Boolean(
  -- * Types
  Boolean,
  BooleanRef,
  -- * Conversion
  toBool,
  fromBool,
  ) where

#include <CoreFoundation/CoreFoundation.h>

import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.IO.Unsafe (unsafePerformIO)
import qualified Foreign
import Foreign.C.Types
import Data.Typeable
import Control.DeepSeq

declareCFType "Boolean"
{#pointer CFBooleanRef as BooleanRef nocode#}

-- | Convert to 'Bool'
{#fun pure unsafe CFBooleanGetValue as toBool
  {withObject* `Boolean'} -> `Bool' 'Foreign.toBool' #}

-- | Convert from 'Bool'
fromBool :: Bool -> Boolean
fromBool True = kTrue
fromBool False = kFalse

kTrue, kFalse :: Boolean
kTrue = unsafePerformIO $ getAndRetain kCFBooleanTrue
kFalse = unsafePerformIO $ getAndRetain kCFBooleanFalse

foreign import ccall "&" kCFBooleanTrue :: BooleanRef
foreign import ccall "&" kCFBooleanFalse :: BooleanRef

deriving instance Typeable Boolean
instance Show Boolean where
  show = show . toBool
instance Eq Boolean where
  a == b = toBool a == toBool b
instance Ord Boolean where
  compare a b = compare (toBool a) (toBool b)
instance NFData Boolean
