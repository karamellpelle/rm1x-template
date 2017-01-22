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

import Template.Types
import Template.File
import Helpers
--import Text.XML.Simple

data SVGPages =
    SVGPages
    {
        name :: String,
        svg0 :: FilePath,
        svg1 :: FilePath,
        svg2 :: FilePath
    }

-- | make 3 svg images from kit
makeSVGPages :: FilePath -> FilePath -> IO (Maybe SVGPages)
makeSVGPages path path' = do
    undefined
    --tm <- readTemplateMap path
    --
    --template <- getDataFileName' "template.svg"
    --
    --readFile template >>= \str -> case parseXMLDoc str of
    --    Nothing   -> return Nothing
    --    Just xml  -> do
    --        undefined
{--
    <flowRoot  transform="translate(47.115584,561.70974)"
               style="font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:7.5px;line-height:125%;font-family:'PT Mono';-inkscape-font-specification:'PT Mono Bold';text-align:start;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:start;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
               id="flowRoot15741"
               xml:space="preserve">
          <flowRegion id="flowRegion15743">
              <rect  style="font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-family:'PT Mono';-inkscape-font-specification:'PT Mono Bold'"
                     y="373.80042"
                     x="0.25021923"
                     height="50.043839"
                     width="50.043846"
                     id="rect15745" />
          </flowRegion>
          <flowPara id="flowPara15747">
              Snare Hard
          </flowPara>
    </flowRoot>
--}
