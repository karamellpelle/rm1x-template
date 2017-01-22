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
module Helpers
  (
    getDataFileName',

    module System.IO,
    module System.Process,
    --module System.IO.Temp,
    module System.Environment,
    module System.Directory,
    module System.FilePath,
    module System.Exit,
    module Control.Monad,
    module Data.Char,
    module Data.List,
    module Data.Maybe,

  ) where

import System.IO
import System.Process
--import System.IO.Temp
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Paths_rm1x_template

--------------------------------------------------------------------------------
--  

-- | this lets us retrieve files without being installed
getDataFileName' :: FilePath -> IO FilePath
getDataFileName' path = do
    path' <- getDataFileName path
    doesFileExist path' >>= \exist ->
        if exist then return path'
                 else return $ "data/" ++ path

