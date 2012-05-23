{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-- | Utilities to parse the type names used by LLVM.  Names are parsed
-- into the representation used by the Itanium ABI package.  This
-- representation can deal with namespace qualified names and supports
-- conversion between Strings and Names.
module LLVM.Analysis.Util.Names (
  parseTypeName,
  unparseTypeName
  ) where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH

import ABI.Itanium

$(derivePrinterParsers ''Name)
$(derivePrinterParsers ''CVQualifier)
$(derivePrinterParsers ''Prefix)
$(derivePrinterParsers ''UnqualifiedName)
$(derivePrinterParsers ''UName)

parseTypeName :: String -> Either String Name
parseTypeName s =
  case parseString name s of
    Right n -> Right n
    Left e -> Left (show e)

unparseTypeName :: Name -> Maybe String
unparseTypeName = unparseString name

name :: PrinterParser StringError String a (Name :- a)
name = ( rNestedName . rList qualifier . rList prefix . unqName <>
         rUnscopedName . unscopedName
       )

unscopedName :: PrinterParser StringError String a (UName :- a)
unscopedName = rUName . unqName

unqName :: PrinterParser StringError String a (UnqualifiedName :- a)
unqName = rSourceName . rList1 (satisfy (/= ':'))

-- Just a hack since we know we won't have qualifiers.  It is fine if
-- it always fails because the empty list is allowed
qualifier :: PrinterParser StringError String a (CVQualifier :- a)
qualifier = rConst . lit "@@INVALID@@"

prefix :: PrinterParser StringError String a (Prefix :- a)
prefix = rUnqualifiedPrefix . unqName . lit "::"