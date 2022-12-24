{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

 module HsBlocks (Block (..), runBlocks) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Either
import Data.IORef
import Data.List
import System.IO
import System.Process
import System.Posix.Signals

data Block = Block
    { icon :: String
    , command :: String
    , refresh :: Either String Int
    }

runBlock :: String -> String -> IORef String -> IO ()
runBlock icon cmd ref = do
    ~(Nothing, Just handle, Nothing, ph) <- createProcess (shell cmd)
        { std_out = CreatePipe
        }
    line <- hGetLine handle
    hClose handle
    _ <- waitForProcess ph
    writeIORef ref (icon ++ ' ' : line)

data Timer = Timer
    { remaining, total :: Int
    , icon :: String
    , command :: String
    , ref :: IORef String
    }

timerThread :: MVar () -> [Timer] -> IO ()
timerThread printSem timers = do
    (timers', Any doPrint) <- runWriterT . forM timers $ \Timer {..} ->
        case remaining of
            0 -> do
                tell (Any True)
                lift (runBlock icon command ref)
                pure Timer {remaining = total - 1, ..}
            _ -> pure Timer {remaining = remaining - 1, ..}
    when doPrint (void (tryPutMVar printSem ()))
    threadDelay 1000000
    timerThread printSem timers'

data Listener = Listener
    { command :: String
    , icon :: String
    , listenerCmd :: String
    , ref :: IORef String
    }

listenerThread :: IORef [ProcessHandle] -> MVar () -> Listener -> IO ()
listenerThread cleanupRef printSem Listener {..} = do
    ~(Nothing, Just handle, Nothing, ph) <- createProcess (shell listenerCmd)
        { std_out = CreatePipe
        }
    atomicModifyIORef' cleanupRef (\other -> (ph : other, ()))
    forever $ do
        runBlock icon command ref
        _ <- tryPutMVar printSem ()
        void (hGetLine handle)

printerThread :: MVar () -> MVar () -> String -> [IORef String] -> IO ()
printerThread _ printSem delim refs = forever $ do
    takeMVar printSem
    strings <- traverse readIORef refs
    putStrLn (intercalate delim strings)
    hFlush stdout

runBlocks :: String -> [Block] -> IO ()
runBlocks delim blocks = do
    pairs <- forM blocks $ \Block {..} -> do
        ref <- newIORef ""
        let refresher = case refresh of
                Right total -> Left Timer {remaining = 0, ..}
                Left listenerCmd -> Right Listener {..}
        pure (ref, refresher)
    let (refs, refreshers) = unzip pairs
    let (timers, listeners) = partitionEithers refreshers
    printSem <- newEmptyMVar
    doneSem <- newEmptyMVar
    cleanupRef <- newIORef []
    mapM_ (forkIO . listenerThread cleanupRef printSem) listeners
    _ <- forkIO (timerThread printSem timers)
    _ <- forkIO (printerThread doneSem printSem delim refs)
    let setDone = putMVar doneSem ()
    _ <- installHandler keyboardSignal (Catch setDone) Nothing
    _ <- installHandler softwareTermination (Catch setDone) Nothing
    takeMVar doneSem
    readIORef cleanupRef >>= mapM_ terminateProcess
