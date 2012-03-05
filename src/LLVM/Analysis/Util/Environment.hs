module LLVM.Analysis.Util.Environment (
  findOpt
  ) where

import System.Directory ( findExecutable )

-- | Find a suitable @opt@ binary in the user's PATH
findOpt :: IO FilePath
findOpt = do
  opt3 <- findExecutable "opt-3.0"
  case opt3 of
    Just e -> return e
    Nothing -> do
      genericOpt <- findExecutable "opt"
      case genericOpt of
        Just e -> return e
        Nothing -> ioError (userError "opt binary not available")