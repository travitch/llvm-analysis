module Data.LLVM.Private.Translators.Metadata ( translateMetadata ) where

import Data.Dwarf
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Maybe ( fromJust )
import Text.Printf

import Data.LLVM.Private.Identifiers
import Data.LLVM.Private.KnotHelpers
import Data.LLVM.Private.DwarfHelpers
import Data.LLVM.Private.PlaceholderTypeExtractors
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Types

import Data.LLVM.Private.ParserOptions

-- Notes on metadata blocks.
--
-- Metadata nodes containing just other metadata nodes are *lists* of
-- metadata nodes referenced from other places.  An MDNode containing
-- any other single value or reference is an argument to
-- llvm.dbg.value noting the new value of a variable.  Any other piece
-- of metadata (besides the source locations handled above) should
-- have an i32 tag as the first argument

-- | The top-level helper to translate metadata values from the
-- non-referential format to the proper referential format during the
-- knot-tying process.  The first value returned is the updated map of
-- 'Identifier's to 'MetaValue's, which has been augmented by the
-- values introduced by this metadata entry.  The second value is an
-- updated mapping of proper 'Value's to their metadata.  This second
-- value is necessary because some constructs (like Function
-- definitions) are not annotated with their metadata.  Instead, the
-- metadata for these constructs declares that it should be attached
-- to some value.
translateMetadata :: ParserOptions ->
                     (O.Constant -> IdStream -> Value) ->
                     (Map Identifier Metadata) ->
                     IdStream ->
                     (Map Identifier Metadata) ->
                     (Map Identifier Metadata) ->
                     Identifier -> [Maybe O.Constant] ->
                     (Map Identifier Metadata, Map Identifier Metadata)
translateMetadata opts trConst allMetadata idStream md valmd name reflist =
  (M.insert name newMetadata md, maybe valmd updateValMDMap valMetadata)
  where
    -- | This helper will determine whether the metadata record has
    -- a tag or not; singleton lists can either be forwarding
    -- records (single metadata entry) or a value reference.  Any
    -- other type of metadata record begins with a *tag*.
    -- Dispatch on this tag to figure out what type of record
    -- should be built.
    (newMetadata, valMetadata) =
      if allRefsMetadata reflist
      then halfPair Metadata { metaValueName = Just name
                             , metaValueContent =
                               MetadataList $ map (metaRef . fromJust) reflist
                             , metaValueUniqueId = extract idStream
                             }
      else case reflist of
        -- Note: only the translateConstant case requires a stream;
        -- all other cases are independent and only require a single
        -- id
        [] -> error "Empty metadata not allowed"
        [Just elt] -> translateConstant idStream elt
        _ -> mkMetadataOrSrcLoc (extract idStream) reflist

    updateValMDMap :: Identifier -> Map Identifier Metadata
    updateValMDMap ident = M.insert ident newMetadata valmd
    -- This helper looks up a metadata reference in the *final* metadata map,
    -- converting a named metadata ref into an actual metadata object
    metaRef (O.ValueRef metaName) = case M.lookup metaName allMetadata of
      Just m -> m
      Nothing -> error $ printf "No metadata node for referenced name %s" (show metaName)
    metaRef c = error $ printf "Constant [%s] is not a metadata reference" (show c)

    allRefsMetadata rs = all isMetadata rs
    isMetadata (Just (O.ValueRef MetaIdentifier {})) = True
    isMetadata _ = False

    translateConstant :: IdStream -> O.Constant -> (Metadata, Maybe Identifier)
    translateConstant is elt =
      halfPair Metadata { metaValueName = Nothing
                        , metaValueContent = mdContent
                        , metaValueUniqueId = extract is
                        }
      where
        is' = split is
        mdContent = MetadataValueConstant (trConst elt is')

    mkMetadataOrSrcLoc :: UniqueId -> [Maybe O.Constant] ->
                          (Metadata, Maybe Identifier)
    mkMetadataOrSrcLoc uid vals@[Just tag, a, b, Nothing] =
      if getInt tag < llvmDebugVersion
      then case metaPositionPrecision opts of
        PositionPrecise -> mkSourceLocation uid name metaRef vals
        PositionNone -> halfPair discardedMetadata
      else mkMetadata uid tag [a, b, Nothing]
    mkMetadataOrSrcLoc uid ((Just tag):rest) = mkMetadata uid tag rest
    mkMetadataOrSrcLoc uid _ = mkUnknown uid name

    -- Here, subtract out the version information from the tag and
    -- construct the indicated record type.  Note: could just
    -- ignore unknown tags.
    mkMetadata :: UniqueId -> O.Constant -> [Maybe O.Constant] -> (Metadata, Maybe Identifier)
    mkMetadata uid tag components = case tag' - llvmDebugVersion of
      1 -> mkCompositeType uid name metaRef DW_TAG_array_type components
      4 -> mkCompositeType uid name metaRef DW_TAG_enumeration_type components
      5 -> mkDerivedType uid name metaRef DW_TAG_formal_parameter components
      11 -> mkLexicalBlock uid name metaRef components
      13 -> mkDerivedType uid name metaRef DW_TAG_member components
      15 -> mkDerivedType uid name metaRef DW_TAG_pointer_type components
      16 -> mkDerivedType uid name metaRef DW_TAG_reference_type components
      17 -> mkCompileUnit uid name components
      19 -> mkCompositeType uid name metaRef DW_TAG_structure_type components
      21 -> mkCompositeType uid name metaRef DW_TAG_subroutine_type components
      22 -> mkDerivedType uid name metaRef DW_TAG_typedef components
      23 -> mkCompositeType uid name metaRef DW_TAG_union_type components
      28 -> mkCompositeType uid name metaRef DW_TAG_inheritance components
      33 -> mkSubrange uid name components
      36 -> mkBaseType uid name metaRef components
      38 -> mkDerivedType uid name metaRef DW_TAG_const_type components
      40 -> mkEnumerator uid name components
      41 -> mkFile uid name metaRef components
      46 -> mkSubprogram uid name metaRef components
      52 -> mkGlobalVar uid name metaRef components
      53 -> mkDerivedType uid name metaRef DW_TAG_volatile_type components
      55 -> mkDerivedType uid name metaRef DW_TAG_restrict_type components
      256 -> mkLocalVar uid name metaRef DW_TAG_auto_variable components
      257 -> mkLocalVar uid name metaRef DW_TAG_arg_variable components
      258 -> mkLocalVar uid name metaRef DW_TAG_return_variable components
      -- These are probably DWARF4 extensions.  FIXME: Re-add them
      -- when the dwarf package supports them

      -- 259 -> mkCompositeType uid name metaRef DW_TAG_vector_type components

      -- There seems to be some kind of metadata generated by
      -- llvm-gcc where the first element is a line number and it
      -- contains two metadata refs.  Not sure what it means,
      -- really.  There are also a few others that make even less
      -- sense.  Just mapping them to something empty for now.
      _ -> mkUnknown uid name


      where tag' = getInt tag

-- | This is a helper to construct a pair with the second element as
-- Nothing; all of the translators return a pair, but most use Nothing
-- for the second value.  The second element of the pair denotes the
-- name of the value that the metadata node is attached to -- only a
-- few types of metadata use this indirect attachment method.
halfPair :: a -> (a, Maybe b)
halfPair x = (x, Nothing)

discardedMetadata :: Metadata
discardedMetadata = Metadata { metaValueName = Just (makeMetaIdentifier "discarded")
                             , metaValueContent = MetadataDiscarded
                             , metaValueUniqueId = -1
                             }

mkUnknown :: UniqueId -> Identifier -> (Metadata, Maybe Identifier)
mkUnknown uid mdValName =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = MetadataUnknown
                     , metaValueUniqueId = uid
                     }

mkSubprogram :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
                (Metadata, Maybe Identifier)
mkSubprogram uid mdValName metaRef [ _, Just context, Just name, Just displayName
                                   , Just linkageName, Just file, Just line
                                   , Just typ, Just isGlobal, Just notExtern
                                   , Just virt, Just virtidx, basetype
                                   , Just isArtif, Just isOpt, mident] =
  (Metadata { metaValueName = Just mdValName
            , metaValueContent = md
            , metaValueUniqueId = uid
            } , maybeIdent mident)
  where
    metaRef' = maybe Nothing (Just . metaRef)
    maybeIdent (Just (O.ValueRef ident)) = Just ident
    maybeIdent Nothing = Nothing
    maybeIdent _ = error ("Invalid subprogram descriptor name " ++ show mident)
    md = MetaDWSubprogram { metaSubprogramContext = metaRef context
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
                          }
mkSubprogram _ _ _ c = error ("Invalid subprogram descriptor: " ++ show c)

mkGlobalVar :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
               (Metadata, Maybe Identifier)
mkGlobalVar uid mdValName metaRef [ _, Just context, Just name, Just displayName
                                  , Just linkageName, Just file, Just line
                                  , Just typ, Just isStatic, Just notExtern
                                  , mident ] =
  (Metadata { metaValueName = Just mdValName
             , metaValueContent = md
             , metaValueUniqueId = uid
             }, maybeIdent mident)
  where
    maybeIdent (Just (O.ValueRef ident)) = Just ident
    maybeIdent Nothing = Nothing
    maybeIdent c = error ("Invalid global variable descriptor: " ++ show c)
    md = MetaDWVariable { metaGlobalVarContext = metaRef context
                        , metaGlobalVarName = getMDString name
                        , metaGlobalVarDisplayName = getMDString displayName
                        , metaGlobalVarLinkageName = getMDString linkageName
                        , metaGlobalVarFile = metaRef file
                        , metaGlobalVarLine = getInt line
                        , metaGlobalVarType = metaRef typ
                        , metaGlobalVarStatic = getBool isStatic
                        , metaGlobalVarNotExtern = getBool notExtern
                        }

mkGlobalVar _ _ _ c = error ("Invalid global variable descriptor: " ++ show c)

mkLocalVar :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> DW_VAR_TAG ->
              [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkLocalVar uid mdValName metaRef tag [ Just context, Just name, Just file
                                     , Just line, Just typeDesc ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }

  where
    md = MetaDWLocal { metaLocalTag = tag
                     , metaLocalContext = metaRef context
                     , metaLocalName = getMDString name
                     , metaLocalFile = metaRef file
                     , metaLocalLine = getInt line
                     , metaLocalType = metaRef typeDesc
                     }
mkLocalVar _ _ _ _ c = error ("Invalid local variable descriptor: " ++ show c)

-- NOTE: Not quite sure what the member descriptor array looks like...
-- FIXME: Also not sure what the last field here is supposed to be.
mkCompositeType :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> DW_TAG ->
                   [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkCompositeType uid mdValName metaRef tag [ Just context, Just name, file, Just line
                                          , Just size, Just align, Just offset, Just flags
                                          , parent, members, Just langs, _ ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    metaRef' = maybe Nothing (Just . metaRef)
    md = MetaDWCompositeType { metaCompositeTypeTag = tag
                             , metaCompositeTypeContext = metaRef context
                             , metaCompositeTypeName = getMDString name
                             , metaCompositeTypeFile = metaRef' file
                             , metaCompositeTypeLine = getInt line
                             , metaCompositeTypeSize = getInt size
                             , metaCompositeTypeAlign = getInt align
                             , metaCompositeTypeOffset = getInt offset
                             , metaCompositeTypeFlags = getInt flags
                             , metaCompositeTypeParent = metaRef' parent
                             , metaCompositeTypeMembers = metaRef' members
                             , metaCompositeTypeRuntime = getInt langs
                             }
mkCompositeType _ _ _ _ c = error ("Invalid composite type descriptor: " ++ show c)

-- FIXME: There is a placeholder here.  The documentation doesn't say
-- what this field is, but it would be really nice to know.
mkDerivedType :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> DW_TAG ->
                 [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkDerivedType uid mdValName metaRef tag [ Just context, Just name, file, Just line
                                        , Just size, Just align, Just offset, _, parent ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    metaRef' = maybe Nothing (Just . metaRef)
    md = MetaDWDerivedType { metaDerivedTypeTag = tag
                           , metaDerivedTypeContext = metaRef context
                           , metaDerivedTypeName = getMDString name
                           , metaDerivedTypeFile = metaRef' file
                           , metaDerivedTypeLine = getInt line
                           , metaDerivedTypeSize = getInt size
                           , metaDerivedTypeAlign = getInt align
                           , metaDerivedTypeOffset = getInt offset
                           , metaDerivedTypeParent = metaRef' parent
                           }
mkDerivedType _ _ _ _ c = error ("Invalid derived type descriptor: " ++ show c)

mkEnumerator :: UniqueId -> Identifier -> [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkEnumerator uid mdValName [ Just name, Just value ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWEnumerator { metaEnumeratorName = getMDString name
                          , metaEnumeratorValue = getInt value
                          }
mkEnumerator _ _ c = error ("Invalid enumerator descriptor content: " ++ show c)

mkSubrange :: UniqueId -> Identifier -> [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkSubrange uid mdValName [ Just low, Just high ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWSubrange { metaSubrangeLow = getInt low
                        , metaSubrangeHigh = getInt high
                        }
mkSubrange _ _ c = error ("Invalid subrange descriptor content: " ++ show c)

mkFile :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
          (Metadata, Maybe Identifier)
mkFile uid mdValName metaRef [ Just file, Just dir, Just unit ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWFile { metaFileSourceFile = getMDString file
                    , metaFileSourceDir = getMDString dir
                    , metaFileCompileUnit = metaRef unit
                    }
mkFile _ _ _ c = error ("Invalid file descriptor content: " ++ show c)

mkSourceLocation :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
                    (Metadata, Maybe Identifier)
mkSourceLocation uid mdValName metaRef [ Just row, Just col, Just scope, Nothing ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaSourceLocation { metaSourceRow = getInt row
                            , metaSourceCol = getInt col
                            , metaSourceScope = metaRef scope
                            }
mkSourceLocation _ _ _ c = error ("Invalid source location content: " ++ show c)


mkLexicalBlock :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
                  (Metadata, Maybe Identifier)
mkLexicalBlock uid mdValName metaRef [ Just context, Just row
                                     , Just col, Just file, Just depth ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWLexicalBlock { metaLexicalBlockContext = metaRef context
                            , metaLexicalBlockRow = getInt row
                            , metaLexicalBlockCol = getInt col
                            , metaLexicalBlockFile = metaRef file
                            , metaLexicalBlockDepth = getInt depth
                            }
mkLexicalBlock _ _ _ c = error ("Invalid lexical block content: " ++ show c)

mkCompileUnit :: UniqueId -> Identifier -> [Maybe O.Constant] -> (Metadata, Maybe Identifier)
mkCompileUnit uid mdValName [ _, Just lang, Just source, Just dir
                            , Just producer, Just isMain
                            , Just isOpt, Just flags, Just version ] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWCompileUnit { metaCompileUnitLanguage = mkDwarfLang $ getInt lang
                           , metaCompileUnitSourceFile = getMDString source
                           , metaCompileUnitCompileDir = getMDString dir
                           , metaCompileUnitProducer = getMDString producer
                           , metaCompileUnitIsMain = getBool isMain
                           , metaCompileUnitIsOpt = getBool isOpt
                           , metaCompileUnitFlags = getMDString flags
                           , metaCompileUnitVersion = getInt version
                           }
mkCompileUnit _ _ c = error ("Invalid compile unit content: " ++ show c)


mkBaseType :: UniqueId -> Identifier -> (O.Constant -> Metadata) -> [Maybe O.Constant] ->
              (Metadata, Maybe Identifier)
mkBaseType uid mdValName metaRef [ Just context, Just name, file, Just line
                                 , Just size, Just align, Just offset
                                 , Just flags, Just dwtype] =
  halfPair Metadata { metaValueName = Just mdValName
                     , metaValueContent = md
                     , metaValueUniqueId = uid
                     }
  where
    md = MetaDWBaseType { metaBaseTypeContext = metaRef context
                        , metaBaseTypeName = getMDString name
                        , metaBaseTypeFile = metaRef' file
                        , metaBaseTypeLine = getInt line
                        , metaBaseTypeSize = getInt size
                        , metaBaseTypeAlign = getInt align
                        , metaBaseTypeOffset = getInt offset
                        , metaBaseTypeFlags = getInt flags
                        , metaBaseTypeEncoding = mkDwarfEncoding $ getInt dwtype
                        }
    metaRef' = maybe Nothing (Just . metaRef)
mkBaseType _ _ _ c = error ("Invalid base type descriptor content: " ++ show c)
