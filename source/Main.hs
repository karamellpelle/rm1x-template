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

import System.IO
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Data.Char
import qualified Data.ByteString.Char8 as BS
import Template

main :: IO ()
main = do
    template <- getDataFileName' "template.svg"
   
    path  <- getOption "input"
    path' <- getOption "output" >>= \path' ->
        doesDirectoryExist path' >>= \is -> if is 
            then return $ path' </> takeBaseName path <.> "svg"
            else return $ dropExtension path' <.> "svg"
    
    -- make sure we have an actual input file
    doesFileExist path >>= \exist -> when (not exist) $ die "Error: Input file does not exist!"

    -- make sure we don't overwrite unintentionally
    doesFileExist path' >>= \exist -> when exist $ do
        putStr $ "Overwrite " ++ path ++ "? (y/N) "
        hFlush stdout
        getLine >>= \a -> 
            case a of 
                ('y':_)  -> return ()
                ('Y':_)  -> return ()
                _        -> putStrLn "Cancelled." >> exitSuccess


    -- read map from file
    -- TODO: catch exception
    tmap <- fmap templateMap $ midiMap path
                    
    putStr $ "Writing file " ++ path' ++ "..."
    hFlush stdout
    
    -- now write file
    replaceTemplate template tmap path path'

    putStrLn "OK"
          


--------------------------------------------------------------------------------
--  

getDataFileName' :: FilePath -> IO FilePath
getDataFileName' path =
    return $ "data/" ++ path
    -- TODO: look if it exist!

--------------------------------------------------------------------------------
--  

getOption :: String -> IO String
getOption opt = do
    args <- getArgs
    case find args opt of 
        Nothing     -> die $ "Error: No --" ++ opt ++ " option given"
        Just ""     -> die $ "Error: Empty --" ++ opt ++ " option"
        Just str    -> return str

    where
      find [] opt     = Nothing
      find (a:as) opt =
          if ( "--" ++ opt == a )
            then case as of 
                    (a':as') -> Just a'
                    _        -> Just ""
            else find as opt



--------------------------------------------------------------------------------
--  

replaceTemplate :: FilePath -> StringMap -> FilePath -> FilePath -> IO ()
replaceTemplate template tmap input output = do

    withFile template ReadMode $ \h -> do
        
        -- create buffer file
        dir <- getTemporaryDirectory
        (tmp, h') <- openTempFile dir $ takeFileName output
        
        --putStrLn ""
        --putStrLn $ "input: " ++ input
        --putStrLn $ "output: " ++ output
        --putStrLn $ "temp: " ++ tmp

        let tmap' = addField tmap "___NAME" $ takeBaseName input
        --forM_ (take 20 map') $ \(x, y) -> putStrLn $ x ++ " -> " ++ y

        -- now go through 'input' and replace each occurence of words in the map
        -- (we assume xml do not use these names for anything else)
        str <- BS.hGetContents h 
        mapM_ (BS.hPut h') $ map (replace tmap') $ split str

        hClose h'
        
        -- now copy temporary file to its destination
        copyFile tmp output

    where
      split = 
          BS.groupBy (\c0 c1 -> pred c0 == pred c1)
      pred c = 
          isAlphaNum c || c == '-' || c == '_' || c == '#'
          
      replace tmap w =
          if isReplaceable w then helper tmap w
                             else w
      helper ((x, x') : xs) w =
          if BS.pack x == w then BS.pack x'
                            else helper xs w
      helper [] w = 
          BS.pack "" -- no mapping for this replacable word
      isReplaceable bs = 
          case BS.unpack bs of
              ('_':'_':'_':c:cs) -> True
              _                  -> False




