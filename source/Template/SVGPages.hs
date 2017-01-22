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
module Template.SVGPages
  (
    SVGPages (..),
    makeSVGPages,

  ) where

import Template.Map
import Template.File
import Helpers
import Text.XML.Light


data SVGPages =
    SVGPages
    {
        svgpagesName :: String,
        svgpagesPath0 :: FilePath,
        svgpagesPath1 :: FilePath,
        svgpagesPath2 :: FilePath
    }

-- | make 3 svg images from kit
makeSVGPages :: FilePath -> FilePath -> IO (Maybe SVGPages)
makeSVGPages kit dir = do
    tm <- readTemplateMap kit

    template <- getDataFileName' "template_pages.svg"
    
    readFile template >>= \str -> do
        
        let xml = parseXML (templatemapSVG tm str)

        let path0 = dir </> (takeBaseName kit ++ "_page0") <.> "svg"
            path1 = dir </> (takeBaseName kit ++ "_page1") <.> "svg"
            path2 = dir </> (takeBaseName kit ++ "_page2") <.> "svg"

        -- write 3 files by modifying the root element
        writeVisibleLayer path0 "___layer0" xml 
        writeVisibleLayer path1 "___layer1" xml 
        writeVisibleLayer path2 "___layer2" xml 
        
        return $ Just $ SVGPages (tmName tm) path0 path1 path2

    where
      writeVisibleLayer path label [prolog, pad, root, pad'] = do
          writeFile path $
              showContent prolog ++
              showContent pad ++
              showContent (visibly label root) ++ -- this is where the magic happens
              showContent pad'
      writeVisibleLayer path label _ = do
          die "Error: Could not understand svg/xml!"

      visibly :: String -> Content -> Content
      visibly label cont@(Elem element) = 
          cont
          -- * find Element with Attribute 'inkscape:label' == label
          -- * change this Element's Attribute 'style' from 'display:none' to 'display:inline'
      visibly label cont =
          cont -- should not happen

