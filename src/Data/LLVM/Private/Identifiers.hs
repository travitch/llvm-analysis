module Data.LLVM.Private.Identifiers ( Identifier(..)
                                     , identifierAsString
                                     , makeLocalIdentifier
                                     , makeGlobalIdentifier
                                     , makeMetaIdentifier
                                     ) where

import Data.Hashable
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)

data Identifier = LocalIdentifier { localIdentifier :: Text
                                  , localHash :: Int
                                  }
                | GlobalIdentifier { globalIdentifier :: Text
                                   , globalHash :: Int
                                   }
                | MetaIdentifier { metaIdentifier :: Text
                                 , metaHash :: Int
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

hsh :: Text -> Int
hsh = hash . encodeUtf8

makeLocalIdentifier :: Text -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { localIdentifier = t
                  , localHash = hsh t
                  }

makeGlobalIdentifier :: Text -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { globalIdentifier = t
                   , globalHash = hsh t
                   }

makeMetaIdentifier :: Text -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { metaIdentifier = t
                 , metaHash = hsh t
                 }

identifierAsString :: Identifier -> String
identifierAsString LocalIdentifier { localIdentifier = t } = unpack t
identifierAsString GlobalIdentifier { globalIdentifier = t } = unpack t
identifierAsString MetaIdentifier { metaIdentifier = t } = unpack t
