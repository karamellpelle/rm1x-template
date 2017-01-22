--    rm1x-template: make keymaps for Yamaha Rm1X
--    Copyright (C) 2017  karamellpelle@hotmail.com
--
--    This program is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License along
--    with this program; if not, write to the Free Software Foundation, Inc.,
--    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
module Main where

import Helpers
import Template


--------------------------------------------------------------------------------
--  

mainHelp :: IO ()
mainHelp = do
    putStrLn "Usage:"
    putStrLn "rm1x-template --svg kit.txt [--output file|folder]"
    putStrLn "rm1x-template --svg-pages kit.txt [--output folder]"
    putStrLn "rm1x-template --book kit1.txt ... kitN.txt [--output file|folder]"


--------------------------------------------------------------------------------
--  main

main :: IO ()
main = do

    help      <- getOption "help"
    svg       <- getOption "svg"
    svgpages  <- getOption "svg-pages"
    book      <- getOption "book"

    when ( isJust help || (isNothing svg && isNothing svgpages && isNothing book) ) $ mainHelp

    case svg of 
        Just []       -> die "No kit input file given."
        Just [path]   -> mainSVG path
        Just _        -> die "Command takes only 1 argument."
        Nothing       -> return ()

    when (isJust svgpages)  $ mainSVGPages (fromJust svgpages)
    when (isJust book)      $ mainBook (fromJust book)



--------------------------------------------------------------------------------
--  


-- | create svg image
mainSVG :: String -> IO ()
mainSVG path = do

    output  <- getOption "output"
    path'   <- case output of
               Just [path'] -> doesDirectoryExist path' >>= \exist -> if exist 
                               then return $ path' </> takeBaseName path <.> "svg"
                               else return path'

               _            -> return $ takeBaseName path <.> "svg"
                               
                               
    
    -- make sure we have valid input and output
    assertExist path
    assertOverwrite path'

    -- make file
    makeSVG path path' >>= \res -> case res of
        Just (SVG name svg) -> putStrLn $ name ++ " (" ++ svg ++ ") written."
        Nothing             -> putStrLn $ "Error: Could not make SVG."
              


mainSVGPages :: [String] -> IO ()
mainSVGPages args = do
    undefined

mainBook :: [String] -> IO ()
mainBook args = do
    undefined


--------------------------------------------------------------------------------
--  

-- | make sure we have an actual input file
assertExist :: FilePath -> IO ()
assertExist path = do
    exist <- doesFileExist path 
    when (not exist) $ die $ "Error: Input file " ++ path ++ " does not exist!"


-- | make sure we don't overwrite unintentionally
assertOverwrite :: FilePath -> IO ()
assertOverwrite path = do
    exist <- doesFileExist path
    when exist $ yesno ("Overwrite " ++ path ++ "? (y/n) ")
                 (return ()) 
                 (putStrLn "Cancelled." >> exitSuccess)

    where
      yesno str y n = do
        putStr str
        hFlush stdout
        getLine >>= \a -> case a of 
            ('y':_)  -> y
            ('Y':_)  -> y
            ('n':_)  -> n
            ('N':_)  -> n
            _        -> yesno str y n

      

--------------------------------------------------------------------------------
--  

getOption :: String -> IO (Maybe [String])
getOption opt = do
    args <- getArgs
    case dropWhile (/= ("--" ++ opt)) args of
        []    -> return Nothing
        as    -> return $ Just $ takeWhile (not . isCommand) $ tail as

    where
      isCommand ('-':'-':c:_) = True
      isCommand _             = False

getFlag :: String -> IO Bool
getFlag opt =
    getOption opt >>= \maybe -> case maybe of
        Just []   -> return True
        _         -> return False





