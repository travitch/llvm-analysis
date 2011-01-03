module Data.LLVM.Private.MetadataTranslator ( translateMetadata ) where

import qualified Data.Map as M
import Data.Map (Map, (!))

import Data.LLVM.Private.DwarfHelpers
import Data.LLVM.Private.PlaceholderTypeExtractors
import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as N

-- Constant defined by LLVM to version tags
llvmDebugVersion = 524288

translateMetadata allMetadata allValues md name reflist isSLoc = M.insert name mdval md
  where mdval = if isSLoc then mkMetaSourceLocation else decodeRefs
        metaRef (O.ValueRef name) = allMetadata ! name
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)
        mkMetaSourceLocation =
          N.MetaSourceLocation { N.metaSourceRow = getInt (reflist !! 0)
                               , N.metaSourceCol = getInt (reflist !! 1)
                               , N.metaSourceScope = metaRef (reflist !! 2)
                               }
        decodeRefs = case reflist of
          [] -> error "Empty metadata not allowed"
          [elt] -> mkMDAliasOrValue elt
          tag:rest -> mkMetadata tag rest
        mkMDAliasOrValue vref@(O.ValueRef name) = metaRef vref
        -- FIXME: Uncomment after implementing generic value translation
        -- mkMDAliasOrValue val = MetaNewValue (translate val)

        -- Here, subtract out the version information from the
        -- tag.
        mkMetadata tag components = case (getInt tag) - llvmDebugVersion of
          11 -> N.MetaDWLexicalBlock { N.metaLexicalBlockRow = getInt (components !! 1)
                                     , N.metaLexicalBlockCol = getInt (components !! 2)
                                     , N.metaLexicalBlockContext = metaRef (components !! 0)
                                     }
          17 -> N.MetaDWCompileUnit { N.metaCompileUnitLanguage = mkDwarfLang $ getInt (components !! 1)
                                    , N.metaCompileUnitSourceFile = getMDString (components !! 2)
                                    , N.metaCompileUnitCompileDir = getMDString (components !! 3)
                                    , N.metaCompileUnitProducer = getMDString (components !! 4)
                                    , N.metaCompileUnitIsMain = getBool (components !! 5)
                                    , N.metaCompileUnitIsOpt = getBool (components !! 6)
                                    , N.metaCompileUnitFlags = getMDString (components !! 7)
                                    , N.metaCompileUnitVersion = getInt (components !! 8)
                                    }
          41 -> N.MetaDWFile { N.metaFileSourceFile = getMDString (components !! 0)
                             , N.metaFileSourceDir = getMDString (components !! 1)
                             , N.metaFileCompileUnit = metaRef (components !! 2)
                             }
          33 -> N.MetaDWSubrange { N.metaSubrangeLow = getInt (components !! 0)
                                 , N.metaSubrangeHigh = getInt (components !! 1)
                                 }
          40 -> N.MetaDWEnumerator { N.metaEnumeratorName = getMDString (components !! 0)
                                   , N.metaEnumeratorValue = getInt (components !! 1)
                                   }
