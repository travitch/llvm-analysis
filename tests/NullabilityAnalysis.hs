import System.Environment ( getArgs )

import Algebra.Lattice
import Data.List ( foldl' )
import Data.Set ( Set )
import qualified Data.Set as S

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.CFG
import Data.LLVM.Analysis.Dataflow

-- Transfer:
--
-- If the value has not been checked for Null (MaybeNull) and it is
-- used, then it is NotNullable
--
-- If the value is Null and is used, it is NullPtrDereference.
--
-- If the value is not Null (meaning it was checked) and is used, it
-- becomes Nullable.
--
-- If the value is checked for Null, it becomes either NotNull or Null

data NullabilityAnalysis =
  NA { nullPtrs :: Set Value
       -- ^ The pointers currently known to be NULL
     , notNullPtrs :: Set Value
       -- ^ The pointers currently known to be not NULL
     , notNullable :: Set Value
       -- ^ Pointers that are dereferenced before they are known to
       -- not be NULL
     , errorPtrs :: Set Value
       -- ^ Pointers that are dereferenced when they must be NULL
     }
                         | Top
                         deriving (Show, Eq)

emptyNullabilityAnalysis = NA { nullPtrs = S.empty
                              , notNullPtrs = S.empty
                              , notNullable = S.empty
                              , errorPtrs = S.empty
                              }

instance MeetSemiLattice NullabilityAnalysis where
  meet Top s = s
  meet s Top = s
  meet s1 s2 = NA { nullPtrs = nullPtrs s1 `S.intersection` nullPtrs s2
                  , notNullPtrs = notNullPtrs s1 `S.intersection` notNullPtrs s2
                  , notNullable = notNullable s1 `S.union` notNullable s2
                  , errorPtrs = errorPtrs s1 `S.union` errorPtrs s2
                  }

instance BoundedMeetSemiLattice NullabilityAnalysis where
  top = Top

instance DataflowAnalysis NullabilityAnalysis where
  transfer = transferFunc

-- | If this is a successor of a null test, add a fact.  This probably
-- also needs to handle getElementPtr, though that really only
-- calculates addresses.  Really, this will have to consult the
-- results of an alias analysis.
transferFunc :: NullabilityAnalysis -> Value -> [EdgeCondition] -> NullabilityAnalysis
transferFunc na v edges = maybe na' addDerefInfo dereferencedPtr
  where
    na' = addEdgeInformation edges

    addDerefInfo p =
      case (S.member p (nullPtrs na'),
            S.member p (notNullPtrs na')) of
        (True, _) -> na' { errorPtrs = p `S.insert` errorPtrs na' }
        (_, False) -> na' { notNullable = p `S.insert` notNullable na' }
        _ -> na'

    dereferencedPtr = case valueContent v of
      StoreInst _ _ dest@Value { valueType = TypePointer _ } _ -> Just dest
      LoadInst _ src@Value { valueType = TypePointer _ } _ -> Just src
      _ -> Nothing


    addEdgeInformation = foldl' processEdge na
    -- Ignore floating point comparisons - only integer comparisons
    -- are used for pointers.
    processEdge n (TrueEdge cmp) = case valueContent cmp of
      ICmpInst ICmpEq v1 Value { valueContent = ConstantPointerNull } ->
        -- v1 is null
        n { nullPtrs = v1 `S.insert` nullPtrs n }
      ICmpInst ICmpEq Value { valueContent = ConstantPointerNull } v2 ->
        -- v2 is null
        n { nullPtrs = v2 `S.insert` nullPtrs n }
      ICmpInst ICmpNe v1 Value { valueContent = ConstantPointerNull } ->
        -- v1 is not null
        n { notNullPtrs = v1 `S.insert` notNullPtrs n }
      ICmpInst ICmpNe Value { valueContent = ConstantPointerNull } v2 ->
        -- v2 is not null
        n { notNullPtrs = v2 `S.insert` notNullPtrs n }
      _ -> n
    processEdge n (FalseEdge cmp) = case valueContent cmp of
      ICmpInst ICmpEq v1 Value { valueContent = ConstantPointerNull } ->
        -- v1 is not null
        n { notNullPtrs = v1 `S.insert` notNullPtrs n }
      ICmpInst ICmpEq Value { valueContent = ConstantPointerNull } v2 ->
        -- v2 is not null
        n { notNullPtrs = v2 `S.insert` notNullPtrs n }
      ICmpInst ICmpNe v1 Value { valueContent = ConstantPointerNull } ->
        -- v1 is null
        n { nullPtrs = v1 `S.insert` nullPtrs n }
      ICmpInst ICmpNe Value { valueContent = ConstantPointerNull } v2 ->
        -- v2 is null
        n { nullPtrs = v2 `S.insert` nullPtrs n }
      _ -> n
    processEdge n _ = n


main :: IO ()
main = do
  [ inFile ] <- getArgs
  llvmModule <- parseLLVMBitcodeFile defaultParserOptions inFile
  either putStrLn nullAnalysis llvmModule

isArgument :: Value -> Bool
isArgument Value { valueContent = Argument _ } = True
isArgument _ = False

nullAnalysis llvmModule = do
  let fs = moduleFunctions llvmModule
      cfgs = map mkCFG fs
      names = map (functionName . valueContent) fs
      na = emptyNullabilityAnalysis
      res = map (forwardDataflow na) cfgs
      res' = map (\(x, y) -> x y) (zip res (map cfgExitValue cfgs))
      res'' = zip names $ map (S.filter isArgument) (map notNullable res')
  mapM_ (putStrLn . show) res''
  return ()