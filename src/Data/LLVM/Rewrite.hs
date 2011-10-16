{-# LANGUAGE TemplateHaskell #-}
-- | Note: When building IR nodes to insert, unique IDs are not
-- available.  They must be assigned by the knot-tying process to
-- ensure uniqueness.  The values in the new IR nodes will be ignored
-- and can be arbitrary (they will just be re-assigned while
-- re-building the Module).
module Data.LLVM.Rewrite (
  -- * Types
  ModuleRewriterContext,
  ModuleRewriter,
  UInstruction,
  UValue,
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
  ModuleRewriteFailure(..)
  ) where

import Control.Category
import Control.Exception
import Control.Monad.State
import Data.Default
import Data.ByteString ( ByteString )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Ord ( comparing )
import Prelude hiding ( (.), id )

import Data.LLVM.Types

-- | The mapping type for this module
type Map = HashMap

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
      , _rwGlobals :: Map GlobalVariable GlobalVariable
      , _rwExternalVals :: Map ExternalValue ExternalValue
      , _rwExternalFuncs :: Map ExternalFunction ExternalFunction
      , _rwNextId :: UniqueId -- ^ The next ID to assign to
                             -- newly-inserted IR elements.
      }

$(makeLenses [''ModuleRewriterContext])

-- | The Monad in which all Module rewriting occurs.
type ModuleRewriter = State ModuleRewriterContext


-- | An empty context used to prime the RewriteMonad
emptyContext :: Module -> ModuleRewriterContext
emptyContext m = MRC { _rwActions = []
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

-- | A value which doesn't have its UniqueId field set
type UValue = UniqueId -> Value

type UConstant = UniqueId -> Constant

-- | The failures that can occur while rewriting a module.  Some of
-- these are only recognizable in the post-rewrite verification stage.
data ModuleRewriteFailure =
  InstructionAlreadyReplaced !Instruction !Instruction -- ^ The first Inst was already replaced with the second
  | InsertedInstructionAfterTerminator !Instruction -- ^ Attempted to insert an Instruction(1) after a terminator Instruction(2)

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

instructionCurrentBlock i = do
  m <- access rwBlocks
  let nominalBlock = instructionBasicBlock i
  return $ M.lookupDefault nominalBlock nominalBlock m

-- INVARIANT: the instructionBasicBlock field must always point to the
-- oldest block possible (the key in the rwBlocks map)

-- FIXME: Write a helper to update blocks.  It needs to figure out the
-- original block based on the input instruction and keep all of the
-- mappings consistent.  This might be hard - have to differentiate
-- between the parent of a newly-created instruction and an original
-- instruction.

modifyBlockContainingInst currentInst blockRewriter = do
  blockId <- nextId
  _ <- rwMapping %= M.insert (Value bb) (Value newBlock)
  _ <- rwBlocks %= M.insert bb newBlock
  return ()
  where
    bb = instructionCurrentBlock currentInst
    newInsts = blockRewriter (basicBlockInstructions bb)
    newBlock = bb { basicBlockInstructions = newInsts
                  , basicBlockUniqueId = blockId
                  }

-- | Give the new instruction its temporary unique ID and set its
-- enclosing basic block to be the same as the instruction it is being
-- placed near.
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
    uid <- nextId
    let newI = instantiateInstruction currentInst newInst
        replaceInst i | EQ == comparing instructionUniqueId i currentInst = newI
                      | otherwise = i
        blockRewriter = map replaceInst
    modifyBlockContainingInst currentInst blockRewriter

    -- Update the mapping so references to this instruction can be updated
    _ <- rwMapping %= M.insert (Value currentInst) (Value newI)

    return newI
  where
    err x = throw (InstructionAlreadyReplaced currentInst x)

unlessAlreadyReplaced lns k err action = do
  m <- access lns
  case M.lookup k m of
    Nothing -> action
    Just v -> throw (err v)

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

    _ <- rwMapping %= M.insert (Value currentInst) newC

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
    let newI = instantiateInstruction target newInst
        insertBefore i lst
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
      let newI = instantiateInstruction target newInst
          insertAfter i lst
            | EQ == comparing instructionUniqueId i target = i : newI : lst
            | otherwise = i : lst
          blockRewriter = foldr insertBefore []
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
addExternalValue evd = do
  uid <- nextId
  let ev = evdToExternalValue evd uid
  _ <- rwActions %= (RAAddExternalValue ev:)
  return ev

-- | Replace the ExternalValue @ev@ with another ExternalValue
-- described by @evd@, updating all references.  This can be used to
-- modify an existing ExternalValue.
--
-- > replaceExternalValue ev evd
replaceExternalValue :: ExternalValue              -- ^ The external value to replace
                        -> ExternalValueDescriptor -- ^ A description of the replacement alias
                        -> ModuleRewriter ExternalValue
replaceExternalValue ev evd = do
  uid <- nextId
  let ev' = evdToExternalValue evd uid
  _ <- rwActions %= (RAReplaceExternalValue ev ev':)
  return ev'

-- | Remove an 'ExternalValue' replacing all references with a new
-- Value that does not yet exist in the IR.  This would probably be a
-- constant.  The rewriter will give the value a unique ID.
removeExternalValueWithNew :: ExternalValue -- ^ The external value to remove
                              -> UValue     -- ^ The replacement to reference in the Module
                              -> ModuleRewriter Value
removeExternalValueWithNew ev val = do
  uid <- nextId
  let newV = val uid
  _ <- rwActions %= (RARemoveExternalValue ev newV:)
  return newV

-- | Remove a global alias, replacing all remaining uses with an
-- existing value.
removeExternalValue :: ExternalValue
                       -> Value
                       -> ModuleRewriter ()
removeExternalValue ev val = do
  _ <- rwActions %= (RARemoveExternalValue ev val:)
  return ()

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
addExternalFunction efd = do
  uid <- nextId
  let ef = efdToExternal efd uid
  _ <- rwActions %= (RAAddExternalFunction ef:)
  return ef

-- | Replace an 'ExternalFunction' with a new one based on the given
-- 'ExternalFunctionDescriptor'.  Returns the new 'ExternalFunction'.
replaceExternalFunction :: ExternalFunction
                           -> ExternalFunctionDescriptor
                           -> ModuleRewriter ExternalFunction
replaceExternalFunction ef efd = do
  uid <- nextId
  let ef' = efdToExternal efd uid
  _ <- rwActions %= (RAReplaceExternalFunction ef ef':)
  return ef'

-- | Remove an external function and replace all remaining references
-- to it with the given Value (that does not yet exist in the IR).
removeExternalFunctionWithNew :: ExternalFunction
                                 -> UValue
                                 -> ModuleRewriter Value
removeExternalFunctionWithNew ef val = do
  uid <- nextId
  let newV = val uid
  _ <- rwActions %= (RARemoveExternalFunction ef newV:)
  return newV

-- | Remove an external function and replace all remaining references
-- to it with the given Value that already exists in the IR.
removeExternalFunction :: ExternalFunction
                          -> Value
                          -> ModuleRewriter ()
removeExternalFunction ef v = do
  _ <- rwActions %= (RARemoveExternalFunction ef v:)
  return ()

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

-- | Add a new global alias from a 'GlobalAliasDescriptor'
addGlobalAlias :: GlobalAliasDescriptor -> ModuleRewriter GlobalAlias
addGlobalAlias gad = do
  uid <- nextId
  let ga = gadToGlobalAlias gad uid
  _ <- rwActions %= (RAAddGlobalAlias ga:)
  return ga

-- | Replace @gv@ with that described by @gvd@, updating all references.
--
-- > replaceGlobalAlias gv gvd
replaceGlobalAlias :: GlobalAlias -- ^ The global alias to replace
                         -> GlobalAliasDescriptor -- ^ A description of the replacement alias
                         -> ModuleRewriter GlobalAlias
replaceGlobalAlias ga gad = do
  uid <- nextId
  let ga' = gadToGlobalAlias gad uid
  _ <- rwActions %= (RAReplaceGlobalAlias ga ga':)
  return ga'

removeGlobalAliasWithNew :: GlobalAlias -- ^ The global alias to remove
                            -> UValue -- ^ The replacement to reference in the Module
                            -> ModuleRewriter Value
removeGlobalAliasWithNew ga val = do
  uid <- nextId
  let newV = val uid
  _ <- rwActions %= (RARemoveGlobalAlias ga newV:)
  return newV

-- | Remove a global alias, replacing all remaining uses with an
-- existing value.
removeGlobalAlias :: GlobalAlias
                     -> Value
                     -> ModuleRewriter ()
removeGlobalAlias ga val = do
  _ <- rwActions %= (RARemoveGlobalAlias ga val:)
  return ()

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
addGlobalVariable gvd = do
  uid <- nextId
  let gv = gvdToGlobal gvd uid
  _ <- rwActions %= (RAAddGlobal gv:)
  return gv

-- | Replace @gv@ with that described by @gvd@, updating all references.
--
-- > replaceGlobalVariable gv gvd
replaceGlobalVariable :: GlobalVariable -- ^ The global variable to replace
                         -> GlobalVariableDescriptor -- ^ A description of the replacement variable
                         -> ModuleRewriter GlobalVariable
replaceGlobalVariable gv gvd = do
  uid <- nextId
  let gv' = gvdToGlobal gvd uid
  _ <- rwActions %= (RAReplaceGlobal gv gv':)
  return gv'

removeGlobalVariableWithNew :: GlobalVariable -- ^ The global variable to remove
                               -> UValue -- ^ The replacement to reference in the Module
                               -> ModuleRewriter Value
removeGlobalVariableWithNew gv val = do
  uid <- nextId
  let newV = val uid
  _ <- rwActions %= (RARemoveGlobal gv newV:)
  return newV

removeGlobalVariable :: GlobalVariable -- ^ The global variable to remove
                        -> Value -- ^ The replacement to reference in the Module
                        -> ModuleRewriter ()
removeGlobalVariable gv val = do
  _ <- rwActions %= (RARemoveGlobal gv val:)
  return ()



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
rewriteModule :: (Failure ModuleRewriteFailure m)
                 => Module
                 -> ModuleRewriterContext
                 -> m Module
rewriteModule m ctx = undefined
  where
    actions = rwActions ^$ ctx


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