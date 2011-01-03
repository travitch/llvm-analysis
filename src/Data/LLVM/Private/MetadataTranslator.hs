module Data.LLVM.Private.MetadataTranslator ( translateMetadata ) where

import qualified Data.Map as M
import Data.Map (Map, (!))

import Data.LLVM.Private.DwarfHelpers
import Data.LLVM.Private.PlaceholderTypeExtractors
import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as N

-- Constant defined by LLVM to version tags
llvmDebugVersion = 524288

-- Notes on metadata blocks.  An MDNode containing just a reference to
-- other metadata can probably just be collapsed.  An MDNode
-- containing any other single value or reference is an argument to
-- llvm.dbg.value noting the new value of a variable.  Any other piece
-- of metadata (besides the source locations handled above) should
-- have an i32 tag as the first argument

translateMetadata allMetadata allValues md name reflist isSLoc = M.insert name mdval md
  where mdval = if isSLoc then mkMetaSourceLocation else decodeRefs
        -- This helper looks up a metadata reference in the *final* metadata map,
        -- converting a named metadata ref into an actual metadata object
        metaRef (O.ValueRef name) = allMetadata ! name
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)
        -- Turn source location meta records into a source location object;
        -- These are the special records without a type tag and ending with
        -- a literal untyped 'null'
        mkMetaSourceLocation = mkSourceLocation metaRef reflist
        -- This helper will determine whether the metadata record has
        -- a tag or not; singleton lists can either be forwarding
        -- records (single metadata entry) or a value reference.  Any
        -- other type of metadata record begins with a *tag*.
        -- Dispatch on this tag to figure out what type of record
        -- should be built.
        decodeRefs = case reflist of
          [] -> error "Empty metadata not allowed"
          [elt] -> mkMDAliasOrValue elt
          tag:rest -> mkMetadata tag rest
        -- Handle the singleton metadata records
        mkMDAliasOrValue vref@(O.ValueRef name) = metaRef vref
        -- FIXME: Uncomment after implementing generic value translation
        -- mkMDAliasOrValue val = MetaNewValue (translate val)

        -- Here, subtract out the version information from the tag
        -- and construct the indicated record type
        mkMetadata tag components = case (getInt tag) - llvmDebugVersion of
          11 -> mkLexicalBlock metaRef components
          17 -> mkCompileUnit components
          33 -> mkSubrange components
          40 -> mkEnumerator components
          41 -> mkFile metaRef components

mkEnumerator [ name, value ] =
  N.MetaDWEnumerator { N.metaEnumeratorName = getMDString name
                     , N.metaEnumeratorValue = getInt value
                     }
mkEnumerator c = error ("Invalid enumerator descriptor content: " ++ show c)

mkSubrange [ low, high ] =
  N.MetaDWSubrange { N.metaSubrangeLow = getInt low
                   , N.metaSubrangeHigh = getInt high
                   }
mkSubrange c = error ("Invalid subrange descriptor content: " ++ show c)

mkFile metaRef [ file, dir, unit ] =
  N.MetaDWFile { N.metaFileSourceFile = getMDString file
               , N.metaFileSourceDir = getMDString dir
               , N.metaFileCompileUnit = metaRef unit
               }
mkFile _ c = error ("Invalid file descriptor content: " ++ show c)

mkSourceLocation metaRef [ row, col, scope ] =
  N.MetaSourceLocation { N.metaSourceRow = getInt row
                       , N.metaSourceCol = getInt col
                       , N.metaSourceScope = metaRef scope
                       }
mkSourceLocation _ c = error ("Invalid source location content: " ++ show c)

mkLexicalBlock metaRef [ context, row, col ] =
  N.MetaDWLexicalBlock { N.metaLexicalBlockContext = metaRef context
                       , N.metaLexicalBlockRow = getInt row
                       , N.metaLexicalBlockCol = getInt col
                       }
mkLexicalBlock _ c = error ("Invalid lexical block content: " ++ show c)

mkCompileUnit [ lang, source, dir, producer, isMain, isOpt, flags, version ] =
  N.MetaDWCompileUnit { N.metaCompileUnitLanguage = mkDwarfLang $ getInt lang
                      , N.metaCompileUnitSourceFile = getMDString source
                      , N.metaCompileUnitCompileDir = getMDString dir
                      , N.metaCompileUnitProducer = getMDString producer
                      , N.metaCompileUnitIsMain = getBool isMain
                      , N.metaCompileUnitIsOpt = getBool isOpt
                      , N.metaCompileUnitFlags = getMDString flags
                      , N.metaCompileUnitVersion = getInt version
                      }
mkCompileUnit c = error ("Invalid compile unit content: " ++ show c)
