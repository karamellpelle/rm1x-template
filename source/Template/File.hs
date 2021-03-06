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
module Template.File
  (
    readMidiMap,
    readTemplateMap,
  ) where

import Helpers
import Template.Map


-- | read mapping from tones to names
readMidiMap :: FilePath -> IO MidiMap
readMidiMap path = 
    readFile path >>= \file ->
        return $ MidiMap $ makeMap $ lines file
    where
      makeMap ls = foldr step [] ls
      step line xs =
        case words line of 
            (w0:ws@(w1:ws2)) -> (w0, unwords ws) : xs   -- line has at least 2 words
            _                -> xs                      -- do nothing and keep rollin'

    

-- | read mapping from replace holders to names
readTemplateMap :: FilePath -> IO TemplateMap
readTemplateMap path = do
    
    mm <- readMidiMap path
    case mm of 
        MidiMap sm  -> 
            let name = takeBaseName path
            in  return $ TemplateMap name $ map toTemplate $ ("NAME", name) : sm

    where
      toTemplate = \(a, b) -> ("___" ++ a, b)

