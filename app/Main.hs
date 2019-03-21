{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
                 <> help "Traverse directory recursively"
                  )
       <*> option (Just <$> str) ( long "connection-string"
                                <> short 'c'
                                <> value Nothing
                                <> help "Database connection string"
                                <> metavar "DSN"
                                 )
       <*> strOption ( long "table-name"
                    <> value "files"
                    <> showDefault
                    <> help "Database table name"
                    <> metavar "NAME"
                    <> hidden
                     )
       <*> strOption ( long "template"
                    <> value "{{{type}}}\t{{{path}}}\t{{{hash}}}\t{{{size}}}"
                    <> showDefault
                    <> help "Mustache template for console output"
                    <> metavar "TEMPLATE"
                    <> hidden
                     )
       <*> switch ( short 'n'
                 <> help "Do not output the trailing newline"
                 <> hidden
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
