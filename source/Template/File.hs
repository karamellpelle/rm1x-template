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
    fileMapWords,
  ) where

import Helpers
import Template.Types


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
            in  return $ TemplateMap name $ map toTemplate sm

    where
      toTemplate = \(a, b) -> ("___" ++ a, escape b)
      escape :: String -> String
      escape w =
          (flip concatMap) w $ \c -> case c of
              '<'   -> "&lt;"
              '>'   -> "&gt;"
              '&'   -> "&amp;"
              '\''  -> "&apos;"
              '"'   -> "&quot;"
              c     -> [c]

          -- ^ http://www.w3schools.com/XML/xml_syntax.asp

--------------------------------------------------------------------------------
--  fileMap


-- | make a copy with content replaced.
--   assuming path and path' are not the same file.
fileMapWords :: StringMap -> FilePath -> FilePath -> IO FilePath
fileMapWords sm path path' = do
    readFile path >>= \str -> do
        writeFile path' $ concat $ map replace $ split str
    return path'

    where
      split = 
          groupBy (\c0 c1 -> pred c0 == pred c1)
      pred c = 
          isAlphaNum c || c == '-' || c == '_' || c == '#' -- ^ this defines words
          
      replace w =
          if isReplaceable w then helper sm w
                             else w
      helper ((x, x') : xs) w =
          if w == x then x' else helper xs w
      helper [] w = "" -- ^ word was replacable but without mapping

      isReplaceable w = 
          case w of
              ('_':'_':'_':c:cs) -> True
              _                  -> False

