module Data.LLVM.Private.Translators.Metadata ( translateMetadata ) where

import Data.Dwarf
import qualified Data.HamtMap as M
import Data.HamtMap ((!))
import Data.Maybe (fromJust)

import Data.LLVM.Private.KnotHelpers
import Data.LLVM.Private.DwarfHelpers
import Data.LLVM.Private.PlaceholderTypeExtractors
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Types

-- Constant defined by LLVM to version tags
llvmDebugVersion :: Integer
llvmDebugVersion = 524288

-- Notes on metadata blocks.
--
-- Metadata nodes containing just other metadata nodes are *lists* of
-- metadata nodes referenced from other places.  An MDNode containing
-- any other single value or reference is an argument to
-- llvm.dbg.value noting the new value of a variable.  Any other piece
-- of metadata (besides the source locations handled above) should
-- have an i32 tag as the first argument

translateMetadata :: (O.Constant -> IdStream -> Value) ->
                     (Map Identifier Metadata) ->
                     (Map Identifier Metadata) ->
                     (Map Identifier Metadata) ->
                     Identifier -> [Maybe O.Constant]
                     -> (Map Identifier Metadata, Map Identifier Metadata)
translateMetadata trConst allMetadata md valmd name reflist =
  (M.insert name newMetadata md, maybe valmd updateValMDMap valMetadata)
  where (newMetadata, valMetadata) = decodeRefs
        updateValMDMap :: Identifier -> Map Identifier Metadata
        updateValMDMap ident = M.insert ident newMetadata valmd
        -- This helper looks up a metadata reference in the *final* metadata map,
        -- converting a named metadata ref into an actual metadata object
        metaRef (O.ValueRef metaName) = allMetadata ! metaName
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)

        allRefsMetadata = all isMetadata reflist
        isMetadata (Just (O.ValueRef MetaIdentifier {})) = True
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
        decodeRefs :: (Metadata, Maybe Identifier)
        decodeRefs =
          if allRefsMetadata
          then (MetadataList $ map (metaRef . fromJust) reflist, Nothing)
          else case reflist of
            [] -> error "Empty metadata not allowed"
            [Just elt] -> translateConstant elt
            _ -> mkMetadataOrSrcLoc reflist

        -- FIXME: this needs to have real identifiers generated
        translateConstant :: O.Constant -> (Metadata, Maybe Identifier)
        translateConstant elt = (MetadataValueConstant (trConst elt initialStream), Nothing)

        mkMetadataOrSrcLoc :: [Maybe O.Constant] -> (Metadata, Maybe Identifier)
        mkMetadataOrSrcLoc vals@[Just tag, a, b, Nothing] =
          if getInt tag < llvmDebugVersion
          then mkSourceLocation metaRef vals
          else mkMetadata tag [a, b, Nothing]
        mkMetadataOrSrcLoc ((Just tag):rest) = mkMetadata tag rest
        mkMetadataOrSrcLoc vals = error ("Unexpected metadata record format: " ++ show vals)

        -- Here, subtract out the version information from the tag and
        -- construct the indicated record type.  Note: could just
        -- ignore unknown tags.
        mkMetadata :: O.Constant -> [Maybe O.Constant] -> (Metadata, Maybe Identifier)
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
          _ -> error ("Unknown metadata node tag (" ++ show (tag' - llvmDebugVersion) ++ ") / " ++ show tag')
          where tag' = getInt tag

halfPair :: a -> (a, Maybe b)
halfPair x = (x, Nothing)

mkSubprogram :: (O.Constant -> Metadata) ->
                [Maybe O.Constant] ->
                (Metadata, Maybe Identifier)
mkSubprogram metaRef [ _, Just context, Just name, Just displayName
                     , Just linkageName, Just file, Just line
                     , Just typ, Just isGlobal, Just notExtern
                     , Just virt, Just virtidx, basetype
                     , Just isArtif, Just isOpt, Just (O.ValueRef ident)] =
  (MetaDWSubprogram { metaSubprogramContext = metaRef context
                      , metaSubprogramName = getMDString name
                      , metaSubprogramDisplayName = getMDString displayName
                      , metaSubprogramLinkageName = getMDString linkageName
                      , metaSubprogramFile = metaRef file
                      , metaSubprogramLine = getInt line
                      , metaSubprogramType = metaRef typ
                      , metaSubprogramStatic = getBool isGlobal
                      , metaSubprogramNotExtern = getBool notExtern
                      , metaSubprogramVirtuality = mkDwarfVirtuality $ getInt virt
                      , metaSubprogramVirtIndex = getInt virtidx
                      , metaSubprogramBaseType = metaRef' basetype
                      , metaSubprogramArtificial = getBool isArtif
                      , metaSubprogramOptimized = getBool isOpt
                      }, Just ident)
  where metaRef' = maybe Nothing (Just . metaRef)
mkSubprogram _ c = error ("Invalid subprogram descriptor: " ++ show c)

mkGlobalVar :: (O.Constant -> Metadata) ->
               [Maybe O.Constant] ->
               (Metadata, Maybe Identifier)
mkGlobalVar metaRef [ _, Just context, Just name, Just displayName
                    , Just linkageName, Just file, Just line
                    , Just typ, Just isStatic, Just notExtern
                    , Just (O.ValueRef ident) ] =
  (MetaDWVariable { metaGlobalVarContext = metaRef context
                   , metaGlobalVarName = getMDString name
                   , metaGlobalVarDisplayName = getMDString displayName
                   , metaGlobalVarLinkageName = getMDString linkageName
                   , metaGlobalVarFile = metaRef file
                   , metaGlobalVarLine = getInt line
                   , metaGlobalVarType = metaRef typ
                   , metaGlobalVarStatic = getBool isStatic
                   , metaGlobalVarNotExtern = getBool notExtern
                   }, Just ident)

mkGlobalVar _ c = error ("Invalid global variable descriptor: " ++ show c)

mkLocalVar :: (O.Constant -> Metadata) -> DW_VAR_TAG ->
              [Maybe O.Constant] ->
              (Metadata, Maybe Identifier)
mkLocalVar metaRef tag [ Just context, Just name, Just file
                       , Just line, Just typeDesc ] = halfPair $
  MetaDWLocal { metaLocalTag = tag
                , metaLocalContext = metaRef context
                , metaLocalName = getMDString name
                , metaLocalFile = metaRef file
                , metaLocalLine = getInt line
                , metaLocalType = metaRef typeDesc
                }
mkLocalVar _ _ c = error ("Invalid local variable descriptor: " ++ show c)

-- NOTE: Not quite sure what the member descriptor array looks like...
-- FIXME: Also not sure what the last field here is supposed to be.
mkCompositeType :: (O.Constant -> Metadata) -> DW_TAG ->
                   [Maybe O.Constant] ->
                   (Metadata, Maybe Identifier)
mkCompositeType metaRef tag [ Just context, Just name, file, Just line
                            , Just size, Just align, Just offset, Just flags
                            , parent, Just members, Just langs, _ ] = halfPair $
  MetaDWCompositeType { metaCompositeTypeTag = tag
                        , metaCompositeTypeContext = metaRef context
                        , metaCompositeTypeName = getMDString name
                        , metaCompositeTypeFile = metaRef' file
                        , metaCompositeTypeLine = getInt line
                        , metaCompositeTypeSize = getInt size
                        , metaCompositeTypeAlign = getInt align
                        , metaCompositeTypeOffset = getInt offset
                        , metaCompositeTypeFlags = getInt flags
                        , metaCompositeTypeParent = metaRef' parent
                        , metaCompositeTypeMembers = metaRef members
                        , metaCompositeTypeRuntime = getInt langs
                        }
  where metaRef' = maybe Nothing (Just . metaRef)
mkCompositeType _ _ c = error ("Invalid composite type descriptor: " ++ show c)

-- FIXME: There is a placeholder here.  The documentation doesn't say
-- what this field is, but it would be really nice to know.
mkDerivedType :: (O.Constant -> Metadata) -> DW_TAG ->
                 [Maybe O.Constant] ->
                 (Metadata, Maybe Identifier)
mkDerivedType metaRef tag [ Just context, Just name, file, Just line
                          , Just size, Just align, Just offset, _, parent ] = halfPair $
  MetaDWDerivedType { metaDerivedTypeTag = tag
                      , metaDerivedTypeContext = metaRef context
                      , metaDerivedTypeName = getMDString name
                      , metaDerivedTypeFile = metaRef' file
                      , metaDerivedTypeLine = getInt line
                      , metaDerivedTypeSize = getInt size
                      , metaDerivedTypeAlign = getInt align
                      , metaDerivedTypeOffset = getInt offset
                      , metaDerivedTypeParent = metaRef' parent
                      }
  where metaRef' = maybe Nothing (Just . metaRef)
mkDerivedType _ _ c = error ("Invalid derived type descriptor: " ++ show c)

mkEnumerator :: [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkEnumerator [ Just name, Just value ] = halfPair $
  MetaDWEnumerator { metaEnumeratorName = getMDString name
                     , metaEnumeratorValue = getInt value
                     }
mkEnumerator c = error ("Invalid enumerator descriptor content: " ++ show c)

mkSubrange :: [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkSubrange [ Just low, Just high ] = halfPair $
  MetaDWSubrange { metaSubrangeLow = getInt low
                   , metaSubrangeHigh = getInt high
                   }
mkSubrange c = error ("Invalid subrange descriptor content: " ++ show c)

mkFile :: (O.Constant -> Metadata) -> [Maybe O.Constant] ->
          (Metadata, Maybe Identifier)
mkFile metaRef [ Just file, Just dir, Just unit ] = halfPair $
  MetaDWFile { metaFileSourceFile = getMDString file
               , metaFileSourceDir = getMDString dir
               , metaFileCompileUnit = metaRef unit
               }
mkFile _ c = error ("Invalid file descriptor content: " ++ show c)

mkSourceLocation :: (O.Constant -> Metadata) -> [Maybe O.Constant] ->
                    (Metadata, Maybe Identifier)
mkSourceLocation metaRef [ Just row, Just col, Just scope, Nothing ] = halfPair $
  MetaSourceLocation { metaSourceRow = getInt row
                       , metaSourceCol = getInt col
                       , metaSourceScope = metaRef scope
                       }
mkSourceLocation _ c = error ("Invalid source location content: " ++ show c)

mkLexicalBlock :: (O.Constant -> Metadata) -> [Maybe O.Constant] ->
                  (Metadata, Maybe Identifier)
mkLexicalBlock metaRef [ Just context, Just row, Just col, Just file, Just depth ] = halfPair $
  MetaDWLexicalBlock { metaLexicalBlockContext = metaRef context
                     , metaLexicalBlockRow = getInt row
                     , metaLexicalBlockCol = getInt col
                     , metaLexicalBlockFile = metaRef file
                     , metaLexicalBlockDepth = getInt depth
                     }
mkLexicalBlock _ c = error ("Invalid lexical block content: " ++ show c)

mkCompileUnit :: [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkCompileUnit [ _, Just lang, Just source, Just dir, Just producer, Just isMain
              , Just isOpt, Just flags, Just version ] = halfPair $
  MetaDWCompileUnit { metaCompileUnitLanguage = mkDwarfLang $ getInt lang
                      , metaCompileUnitSourceFile = getMDString source
                      , metaCompileUnitCompileDir = getMDString dir
                      , metaCompileUnitProducer = getMDString producer
                      , metaCompileUnitIsMain = getBool isMain
                      , metaCompileUnitIsOpt = getBool isOpt
                      , metaCompileUnitFlags = getMDString flags
                      , metaCompileUnitVersion = getInt version
                      }
mkCompileUnit c = error ("Invalid compile unit content: " ++ show c)


mkBaseType :: (O.Constant -> Metadata) -> [Maybe O.Constant] ->
              (Metadata, Maybe Identifier)
mkBaseType metaRef [ Just context, Just name, file, Just line, Just size
                   , Just align, Just offset, Just flags, Just dwtype] = halfPair $
  MetaDWBaseType { metaBaseTypeContext = metaRef context
                   , metaBaseTypeName = getMDString name
                   , metaBaseTypeFile = metaRef' file
                   , metaBaseTypeLine = getInt line
                   , metaBaseTypeSize = getInt size
                   , metaBaseTypeAlign = getInt align
                   , metaBaseTypeOffset = getInt offset
                   , metaBaseTypeFlags = getInt flags
                   , metaBaseTypeEncoding = mkDwarfEncoding $ getInt dwtype
                   }
  where metaRef' = maybe Nothing (Just . metaRef)
mkBaseType _ c = error ("Invalid base type descriptor content: " ++ show c)