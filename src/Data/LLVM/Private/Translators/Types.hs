module Data.LLVM.Private.Translators.Types ( translateType ) where

import qualified Data.HamtMap as M
import Data.HamtMap ((!))

import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as N

-- Create a map from the placeholder types to the final types.  FIXME:
-- Add support for type uprefs.  The code generator always seems to
-- just use named types, so it doesn't seem crucial.
translateType :: [O.GlobalDeclaration] -> (O.Type -> N.Type)
translateType decls = trans'
  where mapping = namedTrans' decls M.empty
        namedTrans' [] m = m
        namedTrans' (d:rest) m = namedTrans' rest $ case d of
          O.NamedType ident ty -> M.insert ident (trans' ty) m
          _ -> m
        trans' :: O.Type -> N.Type
        trans' t = case t of
          O.TypeInteger i -> N.TypeInteger i
          O.TypeFloat -> N.TypeFloat
          O.TypeDouble -> N.TypeDouble
          O.TypeFP128 -> N.TypeFP128
          O.TypeX86FP80 -> N.TypeX86FP80
          O.TypePPCFP128 -> N.TypePPCFP128
          O.TypeX86MMX -> N.TypeX86MMX
          O.TypeVoid -> N.TypeVoid
          O.TypeLabel -> N.TypeLabel
          O.TypeMetadata -> N.TypeMetadata
          O.TypeArray i t' -> N.TypeArray i (trans' t')
          O.TypeVector i t' -> N.TypeVector i (trans' t')
          O.TypeFunction t' ts' v -> N.TypeFunction (trans' t') (map trans' ts') v
          O.TypeOpaque -> N.TypeOpaque
          O.TypePointer t' -> N.TypePointer (trans' t')
          O.TypeStruct ts' -> N.TypeStruct (map trans' ts')
          O.TypePackedStruct ts' -> N.TypePackedStruct (map trans' ts')
          O.TypeUpref _ -> error "Type uprefs not supported yet"
          O.TypeNamed name -> N.TypeNamed (show name) $ mapping ! name

