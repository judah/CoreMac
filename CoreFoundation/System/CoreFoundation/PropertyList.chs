-- | Property lists. See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFPropertyListRef/Reference/reference.html>
module System.CoreFoundation.PropertyList(
  -- * Types
  Plist,
  PlistRef,
  CFPropertyList,
  -- * CoreFoundation subtypes
  PlistElement,
  toPlist,
  fromPlist,
  -- * Pattern matching
  PlistView(..),
  viewPlist,
  ) where

import Prelude hiding(String)
import qualified Prelude
import Data.Typeable
import Control.DeepSeq

import System.CoreFoundation.Base
import System.CoreFoundation.String
import System.CoreFoundation.Number
import System.CoreFoundation.Boolean
import System.CoreFoundation.Date
import System.CoreFoundation.Data
import System.CoreFoundation.Array
import System.CoreFoundation.Dictionary

import Foreign.Ptr
import Foreign.ForeignPtr

#include <CoreFoundation/CoreFoundation.h>

-- | The CoreFoundation @CFPropertyList@ type
data CFPropertyList
type PlistRef = Ptr CFPropertyList
{#pointer CFPropertyListRef as PlistRef nocode#}

{- |
Wraps the @CFPropertyListRef@ type. This is understood to be a
superclass of all of:

 * 'String'

 * 'Number'

 * 'Boolean'

 * 'Date'

 * 'Data'

 * 'Array' 'Plist'

 * 'Dictionary' 'String' 'Plist'

These can be converted to 'Plist's using 'toPlist', and can be
extracted using either 'fromPlist' or 'viewPlist'.
-}
newtype Plist = Plist { unPlist :: ForeignPtr CFPropertyList }


instance Object Plist where
  type Repr Plist = CFPropertyList
  unsafeObject = Plist
  unsafeUnObject = unPlist
  maybeStaticTypeID _ = Nothing

-- | Private class: don't add more instances!
class Object a => PlistElement a
instance PlistElement String
instance PlistElement Number
instance PlistElement Boolean
instance PlistElement Date
instance PlistElement Data
instance PlistElement (Array Plist)
instance PlistElement (Dictionary String Plist)

-- | Cast to 'Plist'
toPlist :: PlistElement a => a -> Plist
toPlist = castObjectOrError . dyn

-- | Try coercing the 'Plist'
fromPlist :: PlistElement a => Plist -> Maybe a
fromPlist = castObject . dyn

-- | Query the type of the 'Plist'
viewPlist :: Plist -> PlistView
viewPlist (fromPlist -> Just v) = String v
viewPlist (fromPlist -> Just v) = Number v
viewPlist (fromPlist -> Just v) = Boolean v
viewPlist (fromPlist -> Just v) = Date v
viewPlist (fromPlist -> Just v) = Data v
viewPlist (fromPlist -> Just v) = Array v
viewPlist (fromPlist -> Just v) = Dictionary v
viewPlist _ = error "System.CoreFoundation.PropertyList: Unexpected type in Plist"

-- | View of the \"outer level\" of a 'Plist'.
data PlistView
 = String !String
 | Number !Number
 | Boolean !Boolean
 | Date !Date
 | Data !Data
 | Array !(Array Plist)
 | Dictionary !(Dictionary String Plist)
  deriving(Show, Eq, Ord, Typeable)

instance NFData PlistView

instance Show Plist where
  show = show . viewPlist
instance Eq Plist where
  a == b = viewPlist a == viewPlist b
instance Ord Plist where
  compare a b = compare (viewPlist a) (viewPlist b)
instance NFData Plist  
