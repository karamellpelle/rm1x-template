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

import System.Environment
import System.Exit
import Control.Monad
import Template

main :: IO ()
main = do
    args <- getArgs
    template <- getDataFileName "template.svg"
   
    path  <- getOption "input"
    --path' <- getOption "output"
    
    map <- midiMap path
    
    forM_ map $ \(x, y) -> putStrLn $ x ++ " -> " ++ y



--------------------------------------------------------------------------------
--  

getDataFileName :: FilePath -> IO FilePath
getDataFileName path =
    return $ "data/" ++ path

--------------------------------------------------------------------------------
--  

getOption :: String -> IO String
getOption opt = do
    args <- getArgs
    case find args opt of 
        Nothing     -> die $ "no --" ++ opt ++ " option given"
        Just ""     -> die $ "empty --" ++ opt ++ " option"
        Just str    -> return str

    where
      find [] opt     = Nothing
      find (a:as) opt =
          if ( "--" ++ opt == a )
            then case as of 
                    (a':as') -> Just a'
                    _        -> Just ""
            else find as opt



