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
module Template.SVG
  (
    SVG (..),
    makeSVG,

  ) where

import Template.Map
import Template.File
import Helpers
import Text.XML.Light

data SVG =
    SVG
    {
        svgName :: String,
        svgPath :: FilePath
    }


-- | 
makeSVG :: FilePath -> FilePath -> IO (Maybe SVG)
makeSVG kit path = do
    tm <- readTemplateMap kit

    template <- getDataFileName' "template.svg"

    -- TODO
    -- * for each element in template map, find the xml element having this as 'id' attribute
    -- * replace this elements text

    -- for now, just replace 
    readFile template >>= \str -> 
        writeFile path $ templatemap' tm str


    return $ Just $ SVG (tmName tm) path


