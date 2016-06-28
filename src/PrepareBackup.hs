{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Posix.Directory ( DirStream
                              , openDirStream
                              , closeDirStream
                              , readDirStream
                              )
import System.Directory (doesDirectoryExist)
import System.Posix.Files ( getSymbolicLinkStatus
                          , FileStatus
                          , isRegularFile
                          , isDirectory
                          )
import Control.Exception (bracket)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM ( TChan
                              , writeTChan
                              , atomically
                              , readTChan
                              , newTChanIO
                              )
import System.Process ( createProcess
                      , proc
                      , CreateProcess(..)
                      , StdStream(..)
                      , waitForProcess
                      )
import System.IO (hGetChar, Handle, hClose, hGetBuf)
import Foreign.Marshal.Alloc (allocaBytes)

withDirStream :: FilePath -> (DirStream -> IO a) -> IO a
withDirStream path = bracket (openDirStream path) closeDirStream

walkDir :: FilePath -> TChan FilePath -> IO ()
walkDir dir chan = withDirStream dir (dirLoop [])
  where
    dirLoop ops ds = readDirStream ds >>= \case
      [] -> mapM_ wait ops
      "." -> dirLoop ops ds
      ".." -> dirLoop ops ds
      ent -> do
        op <- async $ handleEnt (dir </> ent) chan
        dirLoop (op : ops) ds

data SrcStatus = Unmodified | Modified | Missing deriving Show

data EntType = GitDir | Dir | Src SrcStatus | Reg | Unknown

withSrcProcess :: FilePath -> FilePath -> (Handle -> IO a) -> IO a
withSrcProcess file dir run = bracket open close run'
  where
    open = createProcess $ (proc "src" [ "status", "./" ++ file ]) { cwd = Just dir
                                                           , std_out = CreatePipe
                                                           }
    drain out size buf = do
      count <- hGetBuf out buf size
      if count < size
         then return ()
         else drain out size buf
    close (_, Just out, _, procHandle) = do
      let size = 4096
      allocaBytes size (drain out size)
      hClose out
      waitForProcess procHandle
    run' (_, Just out, _, _) = run out

entType :: FilePath -> FileStatus -> IO EntType
entType path stat
  | (isRegularFile stat) = do
      let dir = takeDirectory path
      let file = takeFileName path
      doesDirectoryExist (dir </> ".src") >>= \case
        True -> withSrcProcess file dir $ \out -> hGetChar out >>= \case
          '?' -> return Reg
          'I' -> return Reg
          'M' -> return $ Src Modified
          '!' -> return $ Src Missing
          '=' -> return $ Src Unmodified
          _ -> error "bad src status"
        False -> return Reg
  | (isDirectory stat) = do
      exists <- doesDirectoryExist $ path </> ".git"
      return $ if exists then GitDir else Dir
  | otherwise = return Unknown

handleEnt :: FilePath -> TChan FilePath -> IO ()
handleEnt path chan = getSymbolicLinkStatus path >>= entType path >>= \case
  GitDir -> atomically $ writeTChan chan ("git: " ++ path)
  Dir -> walkDir path chan
  Src stat -> atomically $ writeTChan chan ("src: " ++ show stat ++ " " ++ path)
  Reg -> atomically $ writeTChan chan path
  Unknown -> error "Unknown!"

handleOutput :: TChan FilePath -> IO ()
handleOutput chan = do
  path <- atomically $ readTChan chan
  case path of
    [] -> return ()
    _ -> putStrLn path >> handleOutput chan

main :: IO ()
main = do
  [dir] <- getArgs
  chan <- newTChanIO
  printThread <- async $ handleOutput chan
  op <- async $ walkDir dir chan
  wait op
  atomically $ writeTChan chan []
  wait printThread
