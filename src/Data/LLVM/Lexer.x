{
module Data.LLVM.Lexer ( lexer, Token(..) ) where

import Data.Binary.IEEE754
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal (w2c)
import Data.Monoid
}

%wrapper "basic-bytestring"

$digit = 0-9
$hexdigit = [$digit a-f A-F]
$alpha = [a-zA-Z]
$startChar = [$alpha \$ \. \_]
$identChar = [$startChar $digit]
$whitespace = [\ \t\b]
-- LLVM String characters are simple - quotes are represented as \22
-- (an ascii escape) so parsing them is simple
$stringChar = [^\"]

@decimal = [$digit]+
@quotedString = \" $stringChar* \"

tokens :-
  \n+ { const TNewline }
  $whitespace+ ;

  -- Identifiers
  "@" $startChar $identChar* { mkGlobalIdent }
  "%" $startChar $identChar* { mkLocalIdent }
  "!" $startChar $identChar* { mkMetadataName }
  -- Unnamed identifiers
  "@" @decimal+ { mkGlobalIdent }
  "%" @decimal+ { mkLocalIdent }
  "!" @decimal+ { mkMetadataName }
  -- Quoted string idents
  "@" @quotedString { mkQGlobalIdent }
  "%" @quotedString { mkQLocalIdent }

  -- Standard literals
  "-"? @decimal { mkIntLit }
  "-"? @decimal "." @decimal ("e" [\+\-]? @decimal)? { mkFloatLit }
  "0x"  $hexdigit+ { mkHexFloatLit 2 }
  "0xK" $hexdigit+ { mkHexFloatLit 3 }
  "0xM" $hexdigit+ { mkHexFloatLit 3 }
  "0xL" $hexdigit+ { mkHexFloatLit 3 }
  "c" @quotedString { TString . unquote }

  -- Operator-like things
  "," { const TComma }
  "=" { const TAssign }
  "*" { const TStar }
  "(" { const TLParen }
  ")" { const TRParen }
  "[" { const TLSquare }
  "]" { const TRSquare }
  "{" { const TLCurl }
  "}" { const TRCurl }
  "<" { const TLAngle }
  ">" { const TRAngle }
  "!" { const TBang }

  -- Linkage Types
  "private" { const TPrivate }
  "linker_private" { const TLinkerPrivate }
  "linker_private_weak" { const TLinkerPrivateWeak }
  "linker_private_weak_def_auto" { const TLinkerPrivateWeakDefAuto }
  "internal" { const TInternal }
  "available_externally" { const TAvailableExternally }
  "linkonce" { const TLinkOnce }
  "weak" { const TWeak }
  "common" { const TCommon }
  "appending" { const TAppending }
  "extern_weak" { const TExternWeak }
  "linkonce_odr" { const TLinkOnceODR }
  "weak_odr" { const TWeakODR }
  "dllimport" { const TDLLImport }
  "dllexport" { const TDLLExport }

{
data Token = TInt Integer
           | TFloat Double
           | TString ByteString
           | TNewline
           -- Operator-like tokens
           | TComma
           | TAssign
           | TStar
           | TLParen
           | TRParen
           | TLSquare
           | TRSquare
           | TLCurl
           | TRCurl
           | TLAngle
           | TRangle
           | TBang
           -- Identifiers
           | TLocalIdent ByteString
           | TGlobalIdent ByteString
           | TMetadataName ByteString
           -- Linkage Types
           | TPrivate
           | TLinkerPrivate
           | TLinkerPrivateWeak
           | TLinkerPrivateWeakDefAuto
           | TInternal
           | TAvailableExternally
           | TLinkOnce
           | TWeak
           | TCommon
           | TAppending
           | TExternWeak
           | TLinkOnceODR
           | TWeakODR
           | TDLLImport
           | TDLLExport
           deriving (Show)

-- Helpers for constructing identifiers
mkGlobalIdent = TGlobalIdent . stripSigil
mkLocalIdent = TLocalIdent . stripSigil
mkMetadataName = TMetadataName . stripSigil
mkQGlobalIdent = TGlobalIdent . unquote . stripSigil
mkQLocalIdent = TLocalIdent . unquote . stripSigil
mkQMetadataName = TMetadataNAme . unquote . stripSigil
stripSigil = BS.tail
unquote = BS.tail . BS.init

-- Helpers for the simple literals
mkIntLit s = TInt $ readBS s
mkFloatLit s = TFloat $ readBS s
-- Drop the first pfxLen characters (0x)
mkHexFloatLit pfxLen s = TFloat $ wordToDouble $ readBS s'
  where s' = "0x" `mappend` (BS.drop pfxLen s)
-- Strip off the leading c and then unquote
mkStringConstant = TString . unquote . BS.tail

readBS :: (Read a) => ByteString -> a
readBS = read . bs2s
bs2s s = map w2c $ BS.unpack s

-- Exported interface
lexer = alexScanTokens

}