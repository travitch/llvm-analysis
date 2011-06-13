module Data.LLVM.Private.Identifiers ( Identifier(..)
                                     , identifierAsString
                                     , makeLocalIdentifier
                                     , makeGlobalIdentifier
                                     , makeMetaIdentifier
                                     ) where

import Control.DeepSeq
import Data.Hashable
import Data.ByteString.Char8 ( ByteString, unpack )

data Identifier = LocalIdentifier { localIdentifier :: ByteString
                                  , localHash :: !Int
                                  }
                | GlobalIdentifier { globalIdentifier :: ByteString
                                   , globalHash :: !Int
                                   }
                | MetaIdentifier { metaIdentifier :: ByteString
                                 , metaHash :: !Int
                                 }
                  deriving (Eq, Ord)

instance Show Identifier where
  show LocalIdentifier { localIdentifier = t } = '%' : unpack t
  show GlobalIdentifier { globalIdentifier = t } = '@' : unpack t
  show MetaIdentifier { metaIdentifier = t } = '!' : unpack t

instance Hashable Identifier where
  hash LocalIdentifier { localHash = h } = h
  hash GlobalIdentifier { globalHash = h } = h
  hash MetaIdentifier { metaHash = h } = h

instance NFData Identifier where
  rnf i@(LocalIdentifier {}) = localIdentifier i `seq` localHash i `seq` i `seq` ()
  rnf i@(GlobalIdentifier {}) = globalIdentifier i `seq` globalHash i `seq` i `seq` ()
  rnf i@(MetaIdentifier {}) = metaIdentifier i `seq` metaHash i `seq` i `seq` ()

makeLocalIdentifier :: ByteString -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { localIdentifier = t
                  , localHash = hash t
                  }

makeGlobalIdentifier :: ByteString -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { globalIdentifier = t
                   , globalHash = hash t
                   }

makeMetaIdentifier :: ByteString -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { metaIdentifier = t
                 , metaHash = hash t
                 }

identifierAsString :: Identifier -> String
identifierAsString LocalIdentifier { localIdentifier = t } = unpack t
identifierAsString GlobalIdentifier { globalIdentifier = t } = unpack t
identifierAsString MetaIdentifier { metaIdentifier = t } = unpack t
