{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
-- | Note: When building IR nodes to insert, unique IDs are not
-- available.  They must be assigned by the knot-tying process to
-- ensure uniqueness.  The values in the new IR nodes will be ignored
-- and can be arbitrary (they will just be re-assigned while
-- re-building the Module).
module Data.LLVM.Rewrite (
  -- * Types
  ModuleRewriterContext,
  ModuleRewriter,
  UConstant,
  UInstruction,
  -- * Instructions
  replaceInstruction,
  removeInstructionWithNew,
  removeInstruction,
  insertInstructionBefore,
  insertInstructionAfter,
  -- * External values
  ExternalValueDescriptor(..),
  defaultExternalValueDescriptor,
  externalValueDescriptorFromExternal,
  addExternalValue,
  replaceExternalValue,
  removeExternalValueWithNew,
  removeExternalValue,
  -- * External functions
  ExternalFunctionDescriptor(..),
  defaultExternalFunctionDescriptor,
  externalFunctionDescriptorFromExternal,
  addExternalFunction,
  replaceExternalFunction,
  removeExternalFunctionWithNew,
  removeExternalFunction,
  -- * Global aliases
  GlobalAliasDescriptor(..),
  defaultGlobalAliasDescriptor,
  globalAliasDescriptorFromAlias,
  addGlobalAlias,
  replaceGlobalAlias,
  removeGlobalAliasWithNew,
  removeGlobalAlias,
  -- * Global variables
  GlobalVariableDescriptor(..),
  defaultGlobalVariableDescriptor,
  globalVariableDescriptorFromGlobal,
  addGlobalVariable,
  replaceGlobalVariable,
  removeGlobalVariableWithNew,
  removeGlobalVariable,
  -- * Driver
  ModuleRewriteFailure(..),
  rewriteModule
  ) where

import Control.Category
import Control.Exception
import Control.Monad.State
import Data.Default
import Data.ByteString ( ByteString )
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S
import Data.Int
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Ord ( comparing )
import Data.Typeable
import Prelude hiding ( (.), id )

import Data.LLVM.Types

-- | The mapping type for this module
type Map = HashMap
type Set = HashSet

-- | This is a structure that describes how the IR should be modified.
-- It collects "diffs" from the current IR that will all be applied at
-- once by 'rewriteModule'.  This structure is built in the
-- ModuleRewriter monad.
--
-- Note that the list of actions needs to be reversed before use.
data ModuleRewriterContext =
  MRC { _rwMapping :: Map Value Value
      , _rwBlocks :: Map BasicBlock BasicBlock
      , _rwAliases :: Map GlobalAlias GlobalAlias
      , _rwRemovedAliases :: Set GlobalAlias
      , _rwAddedAliases :: Set GlobalAlias
      , _rwGlobals :: Map GlobalVariable GlobalVariable
      , _rwRemovedGlobals :: Set GlobalVariable
      , _rwAddedGlobals :: Set GlobalVariable
      , _rwExternalVals :: Map ExternalValue ExternalValue
      , _rwRemovedExternalVals :: Set ExternalValue
      , _rwAddedExternalVals :: Set ExternalValue
      , _rwExternalFuncs :: Map ExternalFunction ExternalFunction
      , _rwRemovedExternalFuncs :: Set ExternalFunction
      , _rwAddedExternalFuncs :: Set ExternalFunction
      , _rwNextId :: UniqueId -- ^ The next ID to assign to
                             -- newly-inserted IR elements.
      }

$(makeLenses [''ModuleRewriterContext])

-- | The Monad in which all Module rewriting occurs.
type ModuleRewriter = State ModuleRewriterContext


-- | An empty context used to prime the RewriteMonad
emptyContext :: Module -> ModuleRewriterContext
emptyContext m = MRC { _rwMapping = M.empty
                     , _rwBlocks = M.empty
                     , _rwAliases = M.empty
                     , _rwRemovedAliases = S.empty
                     , _rwAddedAliases = S.empty
                     , _rwGlobals = M.empty
                     , _rwRemovedGlobals = S.empty
                     , _rwAddedGlobals = S.empty
                     , _rwExternalVals = M.empty
                     , _rwRemovedExternalVals = S.empty
                     , _rwAddedExternalVals = S.empty
                     , _rwExternalFuncs = M.empty
                     , _rwRemovedExternalFuncs = S.empty
                     , _rwAddedExternalFuncs = S.empty
                     , _rwNextId = moduleNextId m
                     }

-- | Get the next UniqueId available
nextId :: ModuleRewriter UniqueId
nextId = do
  thisId <- gets _rwNextId
  _ <- rwNextId %= (+1)
  return thisId

-- | An instruction which doesn't have its UniqueId field set
type UInstruction = UniqueId -> Instruction

type UConstant = UniqueId -> Constant

-- | The failures that can occur while rewriting a module.  Some of
-- these are only recognizable in the post-rewrite verification stage.
data ModuleRewriteFailure =
  InstructionAlreadyReplaced !Instruction !Value -- ^ The first Inst was already replaced with the second
  | GlobalAlreadyReplaced !Value !Value
  | GlobalAlreadyRemoved !Value
  | InsertedInstructionAfterTerminator !Instruction -- ^ Attempted to insert an Instruction(1) after a terminator Instruction(2)
  deriving (Typeable, Show)

instance Exception ModuleRewriteFailure

-- FIXME: Don't forget that inserting or replacing an instruction
-- updates its enclosing basic block.  Every basic block update has to
-- go through a helper consuling rwBlocks to ensure we are updating
-- the latest iteration of the block.  Without this step, independent
-- updates of the same block (e.g., insertAfter on two different
-- instructions in a block) will clobber each other.
--
-- To avoid extra complications, do not update the parent pointers in
-- instructions when changing their basic blocks - this will all be
-- done at once in the rewrite phase.

instructionCurrentBlock :: Instruction -> ModuleRewriter BasicBlock
instructionCurrentBlock i = do
  m <- access rwBlocks
  let Just nominalBlock = instructionBasicBlock i
  return $ M.lookupDefault nominalBlock nominalBlock m

-- INVARIANT: the instructionBasicBlock field must always point to the
-- oldest block possible (the key in the rwBlocks map)

-- FIXME: Write a helper to update blocks.  It needs to figure out the
-- original block based on the input instruction and keep all of the
-- mappings consistent.  This might be hard - have to differentiate
-- between the parent of a newly-created instruction and an original
-- instruction.

modifyBlockContainingInst :: Instruction -- ^ An instruction currently in the block
                             -> ([Instruction] -> [Instruction]) -- ^ A function to rewrite the instruction list in the block
                             -> ModuleRewriter ()
modifyBlockContainingInst currentInst blockRewriter = do
  blockId <- nextId
  bb <- instructionCurrentBlock currentInst
  let newInsts = blockRewriter (basicBlockInstructions bb)
      newBlock = bb { basicBlockInstructions = newInsts
                    , basicBlockUniqueId = blockId
                    }
  _ <- rwMapping %= M.insert (Value bb) (Value newBlock)
  _ <- rwBlocks %= M.insert bb newBlock
  return ()

-- | Give the new instruction its temporary unique ID and set its
-- enclosing basic block to be the same as the instruction it is being
-- placed near.
instantiateInstruction :: Instruction -- ^ An existing instruction which is in the same block as the new instruction should be
                          -> (UniqueId -> Instruction) -- ^ A function yielding an Instruction given a unique id
                          -> ModuleRewriter Instruction
instantiateInstruction currentInst newInst = do
  uid <- nextId
  let newI = newInst uid
  return newI { instructionBasicBlock = instructionBasicBlock currentInst }

-- | Replace instruction @currentInst@ with @newInst@:
--
-- > replaceInstruction currentInst newInst
--
-- Updates all uses of @currentInst@.  If @currentInst@ has already
-- been removed or replaced with something else, this will fail with
-- the exception @InstructionAlreadyReplaced@.
replaceInstruction :: Instruction
                      -> UInstruction
                      -> ModuleRewriter Instruction
replaceInstruction currentInst newInst =
  unlessAlreadyReplaced rwMapping (Value currentInst) err $ do
    newI <- instantiateInstruction currentInst newInst
    let replaceInst i | EQ == comparing instructionUniqueId i currentInst = newI
                      | otherwise = i
        blockRewriter = map replaceInst
    modifyBlockContainingInst currentInst blockRewriter

    -- Update the mapping so references to this instruction can be updated
    _ <- rwMapping %= M.insert (Value currentInst) (Value newI)

    return newI
  where
    err x = throw (InstructionAlreadyReplaced currentInst x)

unlessAlreadyReplaced :: Lens ModuleRewriterContext (Map Value Value)
                         -> Value
                         -> (Value -> ModuleRewriter a)
                         -> ModuleRewriter a
                         -> ModuleRewriter a
unlessAlreadyReplaced lns k err action = do
  m <- access lns
  case M.lookup k m of
    Nothing -> action
    Just v -> err v

-- | Remove @currentInst@ from the instruction stream (cannot remove
-- terminators).  Replaces all references to @currentInst@ with @newValue@.
-- The @newValue@ must either already exist in the IR or also be present
-- in the ModuleRewriter when it is finally applied.
--
-- If @currentInst@ has already been removed or replaced, this
-- function will raise an @InstructionAlreadyReplaced@ exception.
removeInstructionWithNew :: Instruction -> UConstant -> ModuleRewriter Constant
removeInstructionWithNew currentInst newValue =
  unlessAlreadyReplaced rwMapping (Value currentInst) err $ do
    uid <- nextId
    let newC = newValue uid
        removeInst i lst | EQ == comparing instructionUniqueId currentInst i = lst
                         | otherwise = i : lst
        blockRewriter = foldr removeInst []
    modifyBlockContainingInst currentInst blockRewriter

    _ <- rwMapping %= M.insert (Value currentInst) (Value newC)

    return newC
  where
    err x = throw (InstructionAlreadyReplaced currentInst x)

-- | Remove an instruction and replace all uses of it with an existing
-- value.  This value must exist in the IR (or be inserted into the IR
-- before this function is called)
removeInstruction :: Instruction -> Value -> ModuleRewriter ()
removeInstruction currentInst newValue =
  unlessAlreadyReplaced rwMapping (Value currentInst) err $ do
    let removeInst i lst | EQ == comparing instructionUniqueId i currentInst = lst
                         | otherwise = i : lst
        blockRewriter = foldr removeInst []
    modifyBlockContainingInst currentInst blockRewriter

    _ <- rwMapping %= M.insert (Value currentInst) newValue
    return ()
  where
    err x = throw (InstructionAlreadyReplaced currentInst x)


-- | Insert instruction @newInst@ before instruction @target@,
-- returning the new instruction.
--
-- > insertInstructionBefore target newInst
--
-- If @target@ is no longer in the IR (due to being replaced or
-- removed), this function will raise an @InstructionAlreaduReplaced@
-- exception.  If the @target@ is _implicitly_ no longer reachable
-- (because its enclosing BasicBlock or Function has been removed), no
-- exception will be raised.
insertInstructionBefore :: Instruction -> UInstruction -> ModuleRewriter Instruction
insertInstructionBefore target newInst =
  unlessAlreadyReplaced rwMapping (Value target) err $ do
    newI <- instantiateInstruction target newInst
    let insertBefore i lst
          | EQ == comparing instructionUniqueId i target = newI : i : lst
          | otherwise = i : lst
        blockRewriter = foldr insertBefore []
    modifyBlockContainingInst target blockRewriter

    return newI
  where
    err x = throw (InstructionAlreadyReplaced target x)

-- | Insert instruction @newInst@ after @target@:
--
-- > insertInstructionAfter target newInst
--
-- Note, this function will raise an error if @target@ is a terminator
-- instruction.
--
-- FIXME: Change basic blocks to have a list of (Instruction Normal)
-- and a single (Instruction Terminator).  This way, this function can
-- just take an (Instruction Normal) and the others can take
-- (Instruction a).  Maybe just use newtype wrappers for terminators?
insertInstructionAfter :: Instruction -> UInstruction -> ModuleRewriter Instruction
insertInstructionAfter target newInst =
  case instructionIsTerminator target of
    True -> throw (InsertedInstructionAfterTerminator target)
    False -> unlessAlreadyReplaced rwMapping (Value target) err $ do
      newI <- instantiateInstruction target newInst
      let insertAfter i lst
            | EQ == comparing instructionUniqueId i target = i : newI : lst
            | otherwise = i : lst
          blockRewriter = foldr insertAfter []
      modifyBlockContainingInst target blockRewriter
      return newI
  where
    err x = throw (InstructionAlreadyReplaced target x)

-- | A representation of 'ExternalValue's that has not yet been
-- inserted into the IR.  This becomes an 'ExternalValue' when
-- provided with a unique ID for the current Module.
data ExternalValueDescriptor =
  EVD { evdType :: Type           -- ^ The type of the external value
      , evdName :: Identifier     -- ^ The name of the external value
      , evdMetadata :: [Metadata] -- ^ External value metadata
      }

-- | Create a new default 'ExternalValueDescriptor'.  A 'Type' and
-- 'Identifier' are required.
defaultExternalValueDescriptor :: Type -> Identifier -> ExternalValueDescriptor
defaultExternalValueDescriptor t i =
  EVD { evdType = t
      , evdName = i
      , evdMetadata = []
      }

-- | Create a new 'ExternalValueDescriptor' based on an existing
-- 'ExternalValue'
externalValueDescriptorFromExternal :: ExternalValue -> ExternalValueDescriptor
externalValueDescriptorFromExternal ev =
  EVD { evdType = externalValueType ev
      , evdName = externalValueName ev
      , evdMetadata = externalValueMetadata ev
      }

-- | Converts a descriptor into a real ExternalValue
evdToExternalValue :: ExternalValueDescriptor -> UniqueId -> ExternalValue
evdToExternalValue evd uid =
  ExternalValue { externalValueType = evdType evd
                , externalValueName = evdName evd
                , externalValueMetadata = evdMetadata evd
                , externalValueUniqueId = uid
                }

-- | Add a new ExternalValue based on the 'ExternalvalueDescriptor'
addExternalValue :: ExternalValueDescriptor -> ModuleRewriter ExternalValue
addExternalValue = addGlobalEntity rwAddedExternalVals evdToExternalValue

-- | Replace the ExternalValue @ev@ with another ExternalValue
-- described by @evd@, updating all references.  This can be used to
-- modify an existing ExternalValue.
--
-- > replaceExternalValue ev evd
replaceExternalValue :: ExternalValue              -- ^ The external value to replace
                        -> ExternalValueDescriptor -- ^ A description of the replacement alias
                        -> ModuleRewriter ExternalValue
replaceExternalValue = replaceGlobalEntity rwExternalVals evdToExternalValue

-- | Remove an 'ExternalValue' replacing all references with a new
-- Value that does not yet exist in the IR.  This would probably be a
-- constant.  The rewriter will give the value a unique ID.
removeExternalValueWithNew :: ExternalValue -- ^ The external value to remove
                              -> UConstant  -- ^ The replacement to reference in the Module
                              -> ModuleRewriter Constant
removeExternalValueWithNew =
  removeGlobalEntityWithNew rwRemovedExternalVals rwExternalVals

-- | Remove a global alias, replacing all remaining uses with an
-- existing value.
removeExternalValue :: ExternalValue
                       -> Value
                       -> ModuleRewriter ()
removeExternalValue = removeGlobalEntity rwRemovedExternalVals rwExternalVals

-- | A representation of an 'ExternalFunction' that is not yet
-- inserted into the IR.
data ExternalFunctionDescriptor =
  EFD { efdType :: Type
      , efdName :: Identifier
      , efdMetadata :: [Metadata]
      , efdAttrs :: [FunctionAttribute]
      }

-- | Create a new external function descriptor
defaultExternalFunctionDescriptor :: Type -> Identifier -> ExternalFunctionDescriptor
defaultExternalFunctionDescriptor t i =
  EFD { efdType = t
      , efdName = i
      , efdMetadata = def
      , efdAttrs = def
      }

-- | Create an external function descriptor based on an existing
-- 'ExternalFunction'
externalFunctionDescriptorFromExternal :: ExternalFunction -> ExternalFunctionDescriptor
externalFunctionDescriptorFromExternal ef =
  EFD { efdType = externalFunctionType ef
      , efdName = externalFunctionName ef
      , efdMetadata = externalFunctionMetadata ef
      , efdAttrs = externalFunctionAttrs ef
      }

-- | Convert an external function descriptor into a realy
-- 'ExternalFunction' by giving it a unique identifier.
efdToExternal :: ExternalFunctionDescriptor -> UniqueId -> ExternalFunction
efdToExternal efd uid =
  ExternalFunction { externalFunctionType = efdType efd
                   , externalFunctionName = efdName efd
                   , externalFunctionMetadata = efdMetadata efd
                   , externalFunctionUniqueId = uid
                   , externalFunctionAttrs = efdAttrs efd
                   }

-- | Add a new ExternalFunction
addExternalFunction :: ExternalFunctionDescriptor -> ModuleRewriter ExternalFunction
addExternalFunction = addGlobalEntity rwAddedExternalFuncs efdToExternal

-- | Replace an 'ExternalFunction' with a new one based on the given
-- 'ExternalFunctionDescriptor'.  Returns the new 'ExternalFunction'.
replaceExternalFunction :: ExternalFunction
                           -> ExternalFunctionDescriptor
                           -> ModuleRewriter ExternalFunction
replaceExternalFunction = replaceGlobalEntity rwExternalFuncs efdToExternal

-- | Remove an external function and replace all remaining references
-- to it with the given Value (that does not yet exist in the IR).
removeExternalFunctionWithNew :: ExternalFunction
                                 -> UConstant
                                 -> ModuleRewriter Constant
removeExternalFunctionWithNew =
  removeGlobalEntityWithNew rwRemovedExternalFuncs rwExternalFuncs

-- | Remove an external function and replace all remaining references
-- to it with the given Value that already exists in the IR.
removeExternalFunction :: ExternalFunction
                          -> Value
                          -> ModuleRewriter ()
removeExternalFunction = removeGlobalEntity rwRemovedExternalFuncs rwExternalFuncs

-- | A representation of 'GlobalAlias'es that have not yet been
-- inserted into the IR. The rewriter will turn these into real
-- GlobalAliases.
data GlobalAliasDescriptor =
  GAD { gadTarget :: Value               -- ^ The aliasee
      , gadLinkage :: LinkageType        -- ^ Linkage of the alias
      , gadName :: Identifier            -- ^ The name assigned to the alias
      , gadVisibility :: VisibilityStyle -- ^ The visibility of the alias
      , gadMetadata :: [Metadata]        -- ^ Any metadata
      }

defaultGlobalAliasDescriptor :: Value -> Identifier -> GlobalAliasDescriptor
defaultGlobalAliasDescriptor v i =
  GAD { gadTarget = v
      , gadLinkage = def
      , gadName = i
      , gadVisibility = def
      , gadMetadata = def
      }

globalAliasDescriptorFromAlias :: GlobalAlias -> GlobalAliasDescriptor
globalAliasDescriptorFromAlias ga =
  GAD { gadTarget = globalAliasTarget ga
      , gadLinkage = globalAliasLinkage ga
      , gadName = globalAliasName ga
      , gadVisibility = globalAliasVisibility ga
      , gadMetadata = globalAliasMetadata ga
      }

gadToGlobalAlias :: GlobalAliasDescriptor -> UniqueId -> GlobalAlias
gadToGlobalAlias gad uid =
  GlobalAlias { globalAliasTarget = gadTarget gad
              , globalAliasLinkage = gadLinkage gad
              , globalAliasName = gadName gad
              , globalAliasVisibility = gadVisibility gad
              , globalAliasMetadata = gadMetadata gad
              , globalAliasUniqueId = uid
              }

addGlobalEntity :: (Eq b, Hashable b)
                   => Lens ModuleRewriterContext (Set b)
                   -> (a -> UniqueId -> b)
                   -> a
                   -> ModuleRewriter b
addGlobalEntity lns convert descriptor = do
  uid <- nextId
  let entity = convert descriptor uid
  _ <- lns %= S.insert entity
  return entity

-- | Add a new global alias from a 'GlobalAliasDescriptor'
addGlobalAlias :: GlobalAliasDescriptor -> ModuleRewriter GlobalAlias
addGlobalAlias = addGlobalEntity rwAddedAliases gadToGlobalAlias

replaceGlobalEntity :: (Eq b, Hashable b, IsValue b)
                       => Lens ModuleRewriterContext (Map b b)
                       -> (c -> UniqueId -> b)
                       -> b
                       -> c
                       -> ModuleRewriter b
replaceGlobalEntity lns convert entity descriptor = do
  curMap <- access lns
  case M.lookup entity curMap of
    Just e -> throw $ GlobalAlreadyReplaced (Value entity) (Value e)
    Nothing -> do
      uid <- nextId
      let entity' = convert descriptor uid
      -- Update the type-specific map
      _ <- lns %= M.insert entity entity'
      -- Update the map used for knot-tying later which is generically
      -- typed
      _ <- rwMapping %= M.insert (Value entity) (Value entity')
      return entity'

-- | Replace @gv@ with that described by @gvd@, updating all references.
--
-- > replaceGlobalAlias gv gvd
replaceGlobalAlias :: GlobalAlias -- ^ The global alias to replace
                         -> GlobalAliasDescriptor -- ^ A description of the replacement alias
                         -> ModuleRewriter GlobalAlias
replaceGlobalAlias = replaceGlobalEntity rwAliases gadToGlobalAlias

removeGlobalEntityWithNew :: (Eq a, Hashable a, IsValue a)
                      => Lens ModuleRewriterContext (Set a)
                      -> Lens ModuleRewriterContext (Map a a)
                      -> a
                      -> (UniqueId -> Constant)
                      -> ModuleRewriter Constant
removeGlobalEntityWithNew remLns repLns entity constant = do
  repMap <- access repLns
  remSet <- access remLns
  case (M.lookup entity repMap, S.member entity remSet) of
    (Just x, _) -> throw $ GlobalAlreadyReplaced (Value entity) (Value x)
    (_, True) -> throw $ GlobalAlreadyRemoved (Value entity)
    _ -> do
      uid <- nextId
      let newC = constant uid
      _ <- remLns %= S.insert entity
      _ <- rwMapping %= M.insert (Value entity) (Value newC)
      return newC

removeGlobalAliasWithNew :: GlobalAlias -- ^ The global alias to remove
                            -> UConstant -- ^ The replacement to reference in the Module
                            -> ModuleRewriter Constant
removeGlobalAliasWithNew = removeGlobalEntityWithNew rwRemovedAliases rwAliases

removeGlobalEntity :: (Eq a, Hashable a, IsValue a)
                      => Lens ModuleRewriterContext (Set a)
                      -> Lens ModuleRewriterContext (Map a a)
                      -> a
                      -> Value
                      -> ModuleRewriter ()
removeGlobalEntity remLns repLns entity val = do
  repMap <- access repLns
  remSet <- access remLns
  case (M.lookup entity repMap, S.member entity remSet) of
    (Just x, _) -> throw $ GlobalAlreadyReplaced (Value entity) (Value x)
    (_, True) -> throw $ GlobalAlreadyRemoved (Value entity)
    _ -> do
      _ <- remLns %= S.insert entity
      _ <- rwMapping %= M.insert (Value entity) val
      return ()


-- | Remove a global alias, replacing all remaining uses with an
-- existing value.
removeGlobalAlias :: GlobalAlias
                     -> Value
                     -> ModuleRewriter ()
removeGlobalAlias = removeGlobalEntity rwRemovedAliases rwAliases

-- FIXME: Add integrity checks for these (replaced thing already replaced)
-- Handle replacing something that was added here.  Should replacing something
-- that has been removed be an error? PRobably

data GlobalVariableDescriptor =
  GVD { gvdType :: Type
      , gvdName :: Identifier
      , gvdMetadata :: [Metadata]
      , gvdLinkage :: LinkageType
      , gvdVisibility :: VisibilityStyle
      , gvdInitializer :: Maybe Value
      , gvdAlignment :: Int64
      , gvdSection :: Maybe ByteString
      , gvdIsThreadLocal :: Bool
      , gvdIsConstant :: Bool
      }

defaultGlobalVariableDescriptor :: Type -> Identifier -> GlobalVariableDescriptor
defaultGlobalVariableDescriptor t i =
  GVD { gvdType = t
      , gvdName = i
      , gvdMetadata = def
      , gvdLinkage = def
      , gvdVisibility = def
      , gvdInitializer = def
      , gvdAlignment = 0
      , gvdSection = def
      , gvdIsThreadLocal = False
      , gvdIsConstant = False
      }

globalVariableDescriptorFromGlobal :: GlobalVariable -> GlobalVariableDescriptor
globalVariableDescriptorFromGlobal gv =
  GVD { gvdType = globalVariableType gv
      , gvdName = globalVariableName gv
      , gvdMetadata = globalVariableMetadata gv
      , gvdLinkage = globalVariableLinkage gv
      , gvdVisibility = globalVariableVisibility gv
      , gvdInitializer = globalVariableInitializer gv
      , gvdAlignment = globalVariableAlignment gv
      , gvdSection = globalVariableSection gv
      , gvdIsThreadLocal = globalVariableIsThreadLocal gv
      , gvdIsConstant = globalVariableIsConstant gv
      }

gvdToGlobal :: GlobalVariableDescriptor -> UniqueId -> GlobalVariable
gvdToGlobal gvd uid =
  GlobalVariable { globalVariableType = gvdType gvd
                 , globalVariableName = gvdName gvd
                 , globalVariableMetadata = gvdMetadata gvd
                 , globalVariableUniqueId = uid
                 , globalVariableLinkage = gvdLinkage gvd
                 , globalVariableVisibility = gvdVisibility gvd
                 , globalVariableInitializer = gvdInitializer gvd
                 , globalVariableAlignment = gvdAlignment gvd
                 , globalVariableSection = gvdSection gvd
                 , globalVariableIsThreadLocal = gvdIsThreadLocal gvd
                 , globalVariableIsConstant = gvdIsConstant gvd
                 }


-- | Add a new global variable from a 'GlobalVariableDescriptor'
addGlobalVariable :: GlobalVariableDescriptor -> ModuleRewriter GlobalVariable
addGlobalVariable = addGlobalEntity rwAddedGlobals gvdToGlobal

-- | Replace @gv@ with that described by @gvd@, updating all references.
--
-- > replaceGlobalVariable gv gvd
replaceGlobalVariable :: GlobalVariable -- ^ The global variable to replace
                         -> GlobalVariableDescriptor -- ^ A description of the replacement variable
                         -> ModuleRewriter GlobalVariable
replaceGlobalVariable = replaceGlobalEntity rwGlobals gvdToGlobal

removeGlobalVariableWithNew :: GlobalVariable -- ^ The global variable to remove
                               -> UConstant -- ^ The replacement to reference in the Module
                               -> ModuleRewriter Constant
removeGlobalVariableWithNew =
  removeGlobalEntityWithNew rwRemovedGlobals rwGlobals

removeGlobalVariable :: GlobalVariable -- ^ The global variable to remove
                        -> Value -- ^ The replacement to reference in the Module
                        -> ModuleRewriter ()
removeGlobalVariable = removeGlobalEntity rwRemovedGlobals rwGlobals

-- | Perform the Module rewrite that is specified in the built-up
-- rewriting context.  This function needs to perform various
-- integrity checks before actually rewriting.
--
-- * Ensure that the replacements for objects being removed are either
--   in the IR (and not being removed) OR are about to be added.
--
-- * Ensure that no instructions are inserted after terminators
--
-- * Ensure that terminators are not removed (they should be replaced
--   by other terminators)
--
-- * SSA verification of changes
-- rewriteModule :: (Failure ModuleRewriteFailure m, MonadIO m)
--               => Module
--               -> ModuleRewriteContext
--               -> (forall s. Module s -> m s a)
--               -> a
rewriteModule :: Module
                 -> ModuleRewriterContext
                 -> m Module
rewriteModule m ctx = undefined


{-

* Within a Module, each Instruction references a Value with a UniqueId.

* Local changes reallocate as much of a function as is required to
implement them.  This could be just a BasicBlock

* Every Value that is changed should have an entry in a

    Map UniqueId Value

  to note that the value with a given UniqueId should be replaced with
  another value with the same ID

  For example, inserting an instruction just changes a BasicBlock, so
  the BasicBlock would be entered into the Map.

  Replacing an instruction only requires adding a mapping from the old
  UniqueId to the new instruction in the Map.  The new instruction will
  have the old UniqueId (is that actually necessary? It shouldn't be).

* Top-level changes need additional tracking to know which of the
fields of the Module need additions (replacements are transparent, but
additions and removals are a bit different)

* After this Map is built up from all of the changes specified, the Module
  can be traversed once inside of a call to mfix using the standard knot-tying
  trick to rebuild it.


-}