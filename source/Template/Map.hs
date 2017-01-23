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
module Template.Map
  (
    StringMap,
    MidiMap (..),
    TemplateMap (..),

    templatemapSVG,
    templatemapSVG',

  ) where

import Helpers

-- | String -> String
type StringMap = 
    [(String, String)]

-- | midi (?) notes to name
data MidiMap =
    MidiMap StringMap

-- | map names in svg template to name
data TemplateMap =
    TemplateMap
    {
        tmName :: String,
        tmMap :: StringMap
    }


--------------------------------------------------------------------------------
--  

implTemplatemapSVG :: Bool -> TemplateMap -> String -> String
implTemplatemapSVG clear (TemplateMap name sm) =
    concat . map replace . split 

    where
      split = 
          groupBy (\c0 c1 -> pred c0 == pred c1)
      pred c = 
          isAlphaNum c || c == '-' || c == '_' || c == '#' -- ^ this defines words
          
      replace w =
          if isReplaceable w then helper sm w
                             else w
      helper ((x, x') : xs) w =
          if w == x then escape x' else helper xs w
      helper [] w = if clear then "" else w         -- ^ word was replacable but without mapping

      isReplaceable w = 
          case w of
              ('_':'_':'_':c:cs) -> True
              _                  -> False

      escape w =
          (flip concatMap) w $ \c -> case c of
              '<'   -> "&lt;"
              '>'   -> "&gt;"
              '&'   -> "&amp;"
              '\''  -> "&apos;"
              '"'   -> "&quot;"
              c     -> [c]


          -- ^ http://www.w3schools.com/XML/xml_syntax.asp
        
-- | replace occurences
templatemapSVG :: TemplateMap -> String -> String
templatemapSVG = implTemplatemapSVG False

-- | replace occurences. clear placeholder if no mapping
templatemapSVG' :: TemplateMap -> String -> String
templatemapSVG' = implTemplatemapSVG True
