{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO.Error
import System.Console.GetOpt

import Sync.MerkleTree.Run

main :: IO ()
main = flip catchIOError (putError . show) $
    do args <- getArgs
       let parsedOpts = getOpt (ReturnInOrder parseNonOption) optDescriptions args
       case () of
         () | [_HIDDENT_CLIENT_MODE_OPTION_] == args -> runChild
            | (options,[],[]) <- parsedOpts -> run $ toSyncOptions options
            | (_,_,errs) <- parsedOpts -> printUsageInfo errs
