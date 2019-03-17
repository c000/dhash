{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_dhash

import Data.List (foldl1')
import Hash.Algorithms

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_dhash.version)
    "dhash - directory hash"
    "take hashes of directory contents"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option auto ( long "hash"
                      <> short 'h'
                      <> value MD5
                      <> showDefault
                      <> metavar ( (\x -> "(" <> x <> ")")
                                 . foldl1' (\x y -> x <> "|" <> y)
                                 . map show
                                 $ availableHashAlgorithms)
                      <> help "Hash algorithm"
                       )
       <*> some (strArgument ( help "Input files"
                            <> metavar "FILE..."
                             ))
       <*> switch ( long "recursive"
                 <> short 'r'
                 <> help "traverse directory recursively"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
