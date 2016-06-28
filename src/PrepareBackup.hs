{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))
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
import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM ( TChan
                              , writeTChan
                              , atomically
                              , readTChan
                              , newTChanIO
                              )

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

data EntType = GitDir | Dir | Reg | Unknown

entType :: FilePath -> FileStatus -> IO EntType
entType path stat
  | (isRegularFile stat) = return Reg
  | (isDirectory stat) = do
      exists <- doesDirectoryExist $ path </> ".git"
      return $ if exists then GitDir else Dir
  | otherwise = return Unknown

handleEnt :: FilePath -> TChan FilePath -> IO ()
handleEnt path chan = do
  stat <- getSymbolicLinkStatus path
  entType path stat >>= \case
    GitDir -> atomically $ writeTChan chan ("git: " ++ path)
    Dir -> walkDir path chan
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
  atomically $ writeTChan chan ""
  wait printThread
