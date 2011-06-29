module Data.LLVM.Private.Types.Identifiers (
  -- * Types
  Identifier(..),
  -- * Accessor
  identifierAsString,
  -- * Builders
  makeLocalIdentifier,
  makeGlobalIdentifier,
  makeMetaIdentifier
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.ByteString.Char8 ( ByteString, unpack )

data Identifier = LocalIdentifier { identifierContent :: !ByteString
                                  , identifierHash :: !Int
                                  }
                | GlobalIdentifier { identifierContent :: !ByteString
                                   , identifierHash :: !Int
                                   }
                | MetaIdentifier { identifierContent :: !ByteString
                                 , identifierHash :: !Int
                                 }
                  deriving (Eq, Ord)

instance Show Identifier where
  show LocalIdentifier { identifierContent = t } = '%' : unpack t
  show GlobalIdentifier { identifierContent = t } = '@' : unpack t
  show MetaIdentifier { identifierContent = t } = '!' : unpack t

instance Hashable Identifier where
  hash = identifierHash

instance NFData Identifier where
  rnf i = identifierContent i `seq` identifierHash i `seq` ()

makeLocalIdentifier :: ByteString -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { identifierContent = t
                  , identifierHash = hash t
                  }

makeGlobalIdentifier :: ByteString -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { identifierContent = t
                   , identifierHash = hash t
                   }

makeMetaIdentifier :: ByteString -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { identifierContent = t
                 , identifierHash = hash t
                 }

identifierAsString :: Identifier -> String
identifierAsString = unpack . identifierContent
