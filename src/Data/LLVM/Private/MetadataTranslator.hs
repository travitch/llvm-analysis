module Data.LLVM.Private.MetadataTranslator ( translateMetadata ) where

import Data.Dwarf
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe (maybe, fromJust)

import Data.LLVM.Private.AttributeTypes
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

translateMetadata :: (Map Identifier N.Metadata) ->
                     (Map Identifier N.Metadata) ->
                     (Map Identifier N.Metadata) ->
                     Identifier -> [Maybe O.Constant]
                     -> (Map Identifier N.Metadata, Map Identifier N.Metadata)
translateMetadata allMetadata md valmd name reflist =
  (M.insert name newMetadata md, maybe valmd updateValMDMap valueMapping)
  where (newMetadata, valueMapping) = decodeRefs
        updateValMDMap :: Identifier -> Map Identifier N.Metadata
        updateValMDMap ident = M.insert ident newMetadata valmd
        -- This helper looks up a metadata reference in the *final* metadata map,
        -- converting a named metadata ref into an actual metadata object
        metaRef (O.ValueRef name) = allMetadata ! name
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)
        -- valueRef (O.ValueRef name) = allValues ! name
        -- valueRef c = error ("Constant is not a value reference: " ++ show c)

        allRefsMetadata = all isMetadata reflist
        isMetadata (Just (O.ValueRef (MetaIdentifier _))) = True
        isMetadata _ = False

        -- Turn source location meta records into a source location object;
        -- These are the special records without a type tag and ending with
        -- a literal untyped 'null'
        -- mkMetaSourceLocation = mkSourceLocation metaRef reflist
        -- This helper will determine whether the metadata record has
        -- a tag or not; singleton lists can either be forwarding
        -- records (single metadata entry) or a value reference.  Any
        -- other type of metadata record begins with a *tag*.
        -- Dispatch on this tag to figure out what type of record
        -- should be built.
        decodeRefs :: (N.Metadata, Maybe Identifier)
        decodeRefs =
          if allRefsMetadata
          then (N.MetadataList $ map (metaRef . fromJust) reflist, Nothing)
          else case reflist of
            [] -> error "Empty metadata not allowed"
            [Just elt] -> translateConstant elt
            _ -> mkMetadataOrSrcLoc reflist
        -- Handle the singleton metadata records
        -- mkMDAliasOrValue vref@(O.ValueRef name) = metaRef vref
        -- FIXME: Uncomment after implementing generic value translation
        -- mkMDAliasOrValue val = MetaNewValue (translate val)
        translateConstant :: O.Constant -> (N.Metadata, Maybe Identifier)
        translateConstant elt = (N.MetadataValueConstant, Nothing)

        mkMetadataOrSrcLoc :: [Maybe O.Constant] -> (N.Metadata, Maybe Identifier)
        mkMetadataOrSrcLoc vals@[Just tag, a, b, Nothing] =
          if getInt tag < llvmDebugVersion
          then mkSourceLocation metaRef vals
          else mkMetadata tag [a, b, Nothing]
        mkMetadataOrSrcLoc ((Just tag):rest) = mkMetadata tag rest

        -- Here, subtract out the version information from the tag
        -- and construct the indicated record type
        mkMetadata :: O.Constant -> [Maybe O.Constant] -> (N.Metadata, Maybe Identifier)
        mkMetadata tag components = case tag' - llvmDebugVersion of
          1 -> mkCompositeType metaRef DW_TAG_array_type components
          4 -> mkCompositeType metaRef DW_TAG_enumeration_type components
          5 -> mkDerivedType metaRef DW_TAG_formal_parameter components
          11 -> mkLexicalBlock metaRef components
          13 -> mkDerivedType metaRef DW_TAG_member components
          15 -> mkDerivedType metaRef DW_TAG_pointer_type components
          16 -> mkDerivedType metaRef DW_TAG_reference_type components
          17 -> mkCompileUnit components
          19 -> mkCompositeType metaRef DW_TAG_structure_type components
          21 -> mkCompositeType metaRef DW_TAG_subroutine_type components
          22 -> mkDerivedType metaRef DW_TAG_typedef components
          23 -> mkCompositeType metaRef DW_TAG_union_type components
          28 -> mkCompositeType metaRef DW_TAG_inheritance components
          33 -> mkSubrange components
          36 -> mkBaseType metaRef components
          38 -> mkDerivedType metaRef DW_TAG_const_type components
          40 -> mkEnumerator components
          41 -> mkFile metaRef components
          46 -> mkSubprogram metaRef components
          52 -> mkGlobalVar metaRef components
          53 -> mkDerivedType metaRef DW_TAG_volatile_type components
          55 -> mkDerivedType metaRef DW_TAG_restrict_type components
          256 -> mkLocalVar metaRef DW_TAG_auto_variable components
          257 -> mkLocalVar metaRef DW_TAG_arg_variable components
          258 -> mkLocalVar metaRef DW_TAG_return_variable components
          -- These are probably DWARF4 extensions.  FIXME: Re-add them
          -- when the dwarf package supports them

          -- 259 -> mkCompositeType metaRef DW_TAG_vector_type components
          where tag' = getInt tag

halfPair x = (x, Nothing)

mkSubprogram metaRef [ _, Just context, Just name, Just displayName
                     , Just linkageName, Just file, Just line
                     , Just typ, Just isGlobal, Just notExtern
                     , Just virt, Just virtidx, basetype
                     , Just isArtif, Just isOpt, Just (O.ValueRef ident)] =
  (N.MetaDWSubprogram { N.metaSubprogramContext = metaRef context
                      , N.metaSubprogramName = getMDString name
                      , N.metaSubprogramDisplayName = getMDString displayName
                      , N.metaSubprogramLinkageName = getMDString linkageName
                      , N.metaSubprogramFile = metaRef file
                      , N.metaSubprogramLine = getInt line
                      , N.metaSubprogramType = metaRef typ
                      , N.metaSubprogramStatic = getBool isGlobal
                      , N.metaSubprogramNotExtern = getBool notExtern
                      , N.metaSubprogramVirtuality = mkDwarfVirtuality $ getInt virt
                      , N.metaSubprogramVirtIndex = getInt virtidx
                      , N.metaSubprogramBaseType = metaRef' basetype
                      , N.metaSubprogramArtificial = getBool isArtif
                      , N.metaSubprogramOptimized = getBool isOpt
                     -- , N.metaSubprogramFunction = valueRef ptr
                      }, Just ident)
  where metaRef' = maybe Nothing (Just . metaRef)
mkSubprogram _ c = error ("Invalid subprogram descriptor: " ++ show c)

mkGlobalVar metaRef [ _, Just context, Just name, Just displayName
                    , Just linkageName, Just file, Just line
                    , Just typ, Just isStatic, Just notExtern
                    , Just (O.ValueRef ident) ] =
  (N.MetaDWVariable { N.metaGlobalVarContext = metaRef context
                   , N.metaGlobalVarName = getMDString name
                   , N.metaGlobalVarDisplayName = getMDString displayName
                   , N.metaGlobalVarLinkageName = getMDString linkageName
                   , N.metaGlobalVarFile = metaRef file
                   , N.metaGlobalVarLine = getInt line
                   , N.metaGlobalVarType = metaRef typ
                   , N.metaGlobalVarStatic = getBool isStatic
                   , N.metaGlobalVarNotExtern = getBool notExtern
                                                -- , N.metaGlobalVarRef = valueRef varRef
                   }, Just ident)

mkGlobalVar _ c = error ("Invalid global variable descriptor: " ++ show c)

mkLocalVar metaRef tag [ Just context, Just name, Just file
                       , Just line, Just typeDesc ] = halfPair $
  N.MetaDWLocal { N.metaLocalTag = tag
                , N.metaLocalContext = metaRef context
                , N.metaLocalName = getMDString name
                , N.metaLocalFile = metaRef file
                , N.metaLocalLine = getInt line
                , N.metaLocalType = metaRef typeDesc
                }
mkLocalVar _ _ c = error ("Invalid local variable descriptor: " ++ show c)

-- NOTE: Not quite sure what the member descriptor array looks like...
mkCompositeType metaRef tag [ Just context, Just name, file, Just line
                            , Just size, Just align, Just offset, Just flags
                            , Just parent, Just members, Just langs ] = halfPair $
  N.MetaDWCompositeType { N.metaCompositeTypeTag = tag
                        , N.metaCompositeTypeContext = metaRef context
                        , N.metaCompositeTypeName = getMDString name
                        , N.metaCompositeTypeFile = metaRef' file
                        , N.metaCompositeTypeLine = getInt line
                        , N.metaCompositeTypeSize = getInt size
                        , N.metaCompositeTypeAlign = getInt align
                        , N.metaCompositeTypeOffset = getInt offset
                        , N.metaCompositeTypeFlags = getInt flags
                        , N.metaCompositeTypeParent = metaRef parent
                        , N.metaCompositeTypeMembers = metaRef members
                        , N.metaCompositeTypeRuntime = getInt langs
                        }
  where metaRef' = maybe Nothing (Just . metaRef)
mkCompositeType _ _ c = error ("Invalid composite type descriptor: " ++ show c)

mkDerivedType metaRef tag [ Just context, Just name, file, Just line
                          , Just size, Just align, Just offset, Just parent ] = halfPair $
  N.MetaDWDerivedType { N.metaDerivedTypeTag = tag
                      , N.metaDerivedTypeContext = metaRef context
                      , N.metaDerivedTypeName = getMDString name
                      , N.metaDerivedTypeFile = metaRef' file
                      , N.metaDerivedTypeLine = getInt line
                      , N.metaDerivedTypeSize = getInt size
                      , N.metaDerivedTypeAlign = getInt align
                      , N.metaDerivedTypeOffset = getInt offset
                      , N.metaDerivedTypeParent = metaRef parent
                      }
  where metaRef' = maybe Nothing (Just . metaRef)
mkDerivedType _ _ c = error ("Invalid derived type descriptor: " ++ show c)

mkEnumerator [ Just name, Just value ] = halfPair $
  N.MetaDWEnumerator { N.metaEnumeratorName = getMDString name
                     , N.metaEnumeratorValue = getInt value
                     }
mkEnumerator c = error ("Invalid enumerator descriptor content: " ++ show c)

mkSubrange [ Just low, Just high ] = halfPair $
  N.MetaDWSubrange { N.metaSubrangeLow = getInt low
                   , N.metaSubrangeHigh = getInt high
                   }
mkSubrange c = error ("Invalid subrange descriptor content: " ++ show c)

mkFile metaRef [ Just file, Just dir, Just unit ] = halfPair $
  N.MetaDWFile { N.metaFileSourceFile = getMDString file
               , N.metaFileSourceDir = getMDString dir
               , N.metaFileCompileUnit = metaRef unit
               }
mkFile _ c = error ("Invalid file descriptor content: " ++ show c)

mkSourceLocation metaRef [ Just row, Just col, Just scope ] = halfPair $
  N.MetaSourceLocation { N.metaSourceRow = getInt row
                       , N.metaSourceCol = getInt col
                       , N.metaSourceScope = metaRef scope
                       }
mkSourceLocation _ c = error ("Invalid source location content: " ++ show c)

mkLexicalBlock metaRef [ Just context, Just row, Just col ] = halfPair $
  N.MetaDWLexicalBlock { N.metaLexicalBlockContext = metaRef context
                       , N.metaLexicalBlockRow = getInt row
                       , N.metaLexicalBlockCol = getInt col
                       }
mkLexicalBlock _ c = error ("Invalid lexical block content: " ++ show c)

mkCompileUnit [ _, Just lang, Just source, Just dir, Just producer, Just isMain
              , Just isOpt, Just flags, Just version ] = halfPair $
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



mkBaseType metaRef [ Just context, Just name, file, Just line, Just size
                   , Just align, Just offset, Just flags, Just dwtype] = halfPair $
  N.MetaDWBaseType { N.metaBaseTypeContext = metaRef context
                   , N.metaBaseTypeName = getMDString name
                   , N.metaBaseTypeFile = metaRef' file
                   , N.metaBaseTypeLine = getInt line
                   , N.metaBaseTypeSize = getInt size
                   , N.metaBaseTypeAlign = getInt align
                   , N.metaBaseTypeOffset = getInt offset
                   , N.metaBaseTypeFlags = getInt flags
                   , N.metaBaseTypeEncoding = mkDwarfEncoding $ getInt dwtype
                   }
  where metaRef' = maybe Nothing (Just . metaRef)
mkBaseType _ c = error ("Invalid base type descriptor content: " ++ show c)