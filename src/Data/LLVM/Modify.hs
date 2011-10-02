-- | Note: When building IR nodes to insert, unique IDs are not
-- available.  They must be assigned by the knot-tying process to
-- ensure uniqueness.  The values in the new IR nodes will be ignored
-- and can be arbitrary (they will just be re-assigned while
-- re-building the Module).
module Data.LLVM.Modify where

import Control.Category
import Control.Failure
import Control.Monad.State
import Data.Default
import Data.HashMap.Strict ( HashMap )
import Data.ByteString ( ByteString )
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Lens.Strict
import Prelude hiding ( (.), id )

import Data.LLVM.Types

-- The mapping type for this module
type Map = HashMap

-- | A helper to modify a field in the state of a 'StateMonad' through
-- a lens
modifyL :: MonadState s m => Lens s b -> (b -> b) -> m ()
modifyL lns f = modify (modL lns f)

-- | This is a structure that describes how the IR should be modified.
-- It collects "diffs" from the current IR that will all be applied at
-- once by 'rewriteModule'.  This structure is built in the
-- ModuleRewriter monad.
data ModuleRewriterContext =
  MRC { _rwReplaceInst :: Map Instruction Instruction
      , _rwRemoveInst :: Map Instruction Value
      , _rwInsertInstBefore :: Map Instruction Instruction
      , _rwInsertInstAfter :: Map Instruction Instruction
      , _rwAddGlobal :: [GlobalVariable]
      , _rwReplaceGlobal :: Map GlobalVariable GlobalVariable
      , _rwRemoveGlobal :: Map GlobalVariable Value
      , _rwNextId :: UniqueId
      }

emptyContext :: Module -> ModuleRewriterContext
emptyContext m = MRC { _rwReplaceInst = M.empty
                     , _rwRemoveInst = M.empty
                     , _rwInsertInstBefore = M.empty
                     , _rwInsertInstAfter = M.empty
                     , _rwAddGlobal = []
                     , _rwReplaceGlobal = M.empty
                     , _rwRemoveGlobal = M.empty
                     , _rwNextId = moduleNextId m
                     }

rwReplaceInst :: Lens ModuleRewriterContext (Map Instruction Instruction)
rwReplaceInst = lens _rwReplaceInst (\x s -> s { _rwReplaceInst = x })

rwRemoveInst :: Lens ModuleRewriterContext (Map Instruction Value)
rwRemoveInst = lens _rwRemoveInst (\x s -> s { _rwRemoveInst = x })

rwInsertInstBefore :: Lens ModuleRewriterContext (Map Instruction Instruction)
rwInsertInstBefore = lens _rwInsertInstBefore (\x s -> s { _rwInsertInstBefore = x })

rwInsertInstAfter :: Lens ModuleRewriterContext (Map Instruction Instruction)
rwInsertInstAfter = lens _rwInsertInstAfter (\x s -> s { _rwInsertInstAfter = x })

rwAddGlobal :: Lens ModuleRewriterContext [GlobalVariable]
rwAddGlobal = lens _rwAddGlobal (\x s -> s { _rwAddGlobal = x })

rwReplaceGlobal :: Lens ModuleRewriterContext (Map GlobalVariable GlobalVariable)
rwReplaceGlobal = lens _rwReplaceGlobal (\x s -> s { _rwReplaceGlobal = x })

rwRemoveGlobal :: Lens ModuleRewriterContext (Map GlobalVariable Value)
rwRemoveGlobal = lens _rwRemoveGlobal (\x s -> s { _rwRemoveGlobal = x })

rwNextId :: Lens ModuleRewriterContext Int
rwNextId = lens _rwNextId (\x s -> s { _rwNextId = x })

nextId :: ModuleRewriter Int
nextId = do
  thisId <- gets _rwNextId
  modifyL rwNextId (+1)
  return thisId

type UInstruction = UniqueId -> Instruction
type UValue = UniqueId -> Value

-- ModuleRewriter is a State monad over ModificationContexts
type ModuleRewriter = State ModuleRewriterContext

-- | Replace instruction @currentInst@ with @newInst@:
--
-- > replaceInstruction currentInst newInst
--
-- Updates all uses of @currentInst@.
replaceInstruction :: Instruction -> UInstruction -> ModuleRewriter Instruction
replaceInstruction currentInst newInst = do
  uid <- nextId
  let newI = newInst uid
      addMapping = M.insert currentInst newI
  modifyL rwReplaceInst addMapping
  return newI

-- | Remove @currentInst@ from the instruction stream (cannot remove
-- terminators).  Replaces all references to @currentInst@ with @newValue@.
-- The @newValue@ must either already exist in the IR or also be present
-- in the ModuleRewriter when it is finally applied.
removeInstruction :: Instruction -> UValue -> ModuleRewriter Value
removeInstruction currentInst newValue = do
  uid <- nextId
  let newV = newValue uid
      addMapping = M.insert currentInst newV
  modifyL rwRemoveInst addMapping
  return newV

-- | Insert instruction @newInst@ before instruction @target@:
--
-- > insertInstructionBefore target newInst
insertInstructionBefore :: Instruction -> UInstruction -> ModuleRewriter Instruction
insertInstructionBefore target newInst = do
  uid <- nextId
  let newI = newInst uid
      addMapping = M.insert target newI
  modifyL rwInsertInstBefore addMapping
  return newI

-- | Insert instruction @newInst@ after @target@:
--
-- > insertInstructionAfter target newInst
--
-- Note, this function will raise an error if @target@ is a terminator
-- instruction. FIXME: Change basic blocks to have a list of
-- (Instruction Normal) and a single (Instruction Terminator).  This way,
-- this function can just take an (Instruction Normal) and the others can
-- take (Instruction a).  Maybe just use newtype wrappers for terminators?
insertInstructionAfter :: Instruction -> UInstruction -> ModuleRewriter Instruction
insertInstructionAfter target newInst = do
  uid <- nextId
  let newI = newInst uid
      addMapping = M.insert target newI
  modifyL rwInsertInstAfter addMapping
  return newI

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
  modifyL rwAddGlobal (gv:)
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
      addMapping = M.insert gv gv'
  modifyL rwReplaceGlobal addMapping
  return gv'

removeGlobalVariable :: GlobalVariable -- ^ The global variable to remove
                        -> UValue -- ^ The replacement to reference in the Module
                        -> ModuleRewriter Value
removeGlobalVariable gv val = do
  uid <- nextId
  let newV = val uid
      addMapping = M.insert gv newV
  modifyL rwRemoveGlobal addMapping
  return newV

data ModuleRewriteFailure = ModuleRewriteFailure

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
-- rewriteModule :: (Monad m, Failure ModuleRewriteFailure m)
--                  => Module -- ^ The 'Module' to rewrite
--                  -> ModuleRewriterContext -- ^ A context built in the 'ModuleRewriter' monad
--                  -> m Module -- ^ The rewritten module (or an error)
