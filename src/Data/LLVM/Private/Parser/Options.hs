-- | This module defines options that control the construction of
-- 'Module's.
module Data.LLVM.Private.Parser.Options (
  -- * Types
  ParserOptions(..),
  PositionPrecision(..),
  -- * Helpers
  defaultParserOptions
  ) where

-- | Defines the level of precision of position information in the
-- metadata.  LLVM gives very precise information, but tracking all of
-- it can consume excessive amounts of space.  This option allows it
-- to be selectively discarded.
data PositionPrecision = PositionPrecise
                         -- ^ Preserve all information from LLVM (line
                         -- and column numbers)
                       | PositionNone
                         -- ^ Discard all position information
                       deriving (Show, Eq)

-- | Options controlling how 'Module's are constructed.
data ParserOptions = ParserOptions { metaPositionPrecision :: PositionPrecision }
                   deriving (Show, Eq)

-- | Reasonable default parsing options
defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions { metaPositionPrecision = PositionNone }
