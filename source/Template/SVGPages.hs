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
import XML


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

    --case parseXMLDoc "<g inkscape:groupmode=\"layer\" id=\"layer3\" inkscape:label=\"svgpages___layer0\" style=\"display:inline\">" of
    --case parseXMLDoc "<g inkscape:label=\"svgpages___layer0\" inkscape:groupmode=\"layer\" id=\"layer1\" style=\"display:none\"> <g id=\"g4165\"> <rect y=\"133.12338\" x=\"62.629459\" height=\"189.90868\" width=\"242.43661\" id=\"rect4136\" style=\"opacity:1;fill:none;fill-opacity:1;stroke:#000000;stroke-width:1;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1\" /> <text sodipodi:linespacing=\"125%\" id=\"text4144\" y=\"292.36221\" x=\"177.14285\" style=\"font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:40px;line-height:125%;font-family:sans-serif;-inkscape-font-specification:&#39;sans-serif, Normal&#39;;text-align:center;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" xml:space=\"preserve\"><tspan y=\"292.36221\" x=\"177.14285\" id=\"tspan4146\" sodipodi:role=\"line\">Layer0</tspan></text> </g> <text xml:space=\"preserve\" style=\"font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:40px;line-height:125%;font-family:sans-serif;-inkscape-font-specification:&#39;sans-serif, Normal&#39;;text-align:center;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" x=\"107.14286\" y=\"353.79077\" id=\"text4227\" sodipodi:linespacing=\"125%\"><tspan sodipodi:role=\"line\" x=\"107.14286\" y=\"353.79077\" id=\"tspan4231\">BD Analog Blip</tspan></text> </g>" of
        --Just elem -> do
        --    print $ toQName "id"
        --    print $ toQName "inkscape:label"
        --    print elem >> putStrLn ""
        --    --if elementHasAttribute (toQName "inkscape:label") elem 
        --    --if elementAttributeIs (toQName "inkscape:label") "svgpages___layer0" elem 
        --    if svgElementHasLabel  "svgpages___layer0" elem 
        --       then putStrLn "has layer label"
        --       else putStrLn "fuck"
        --
    --case parseXML "<svg xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:cc=\"http://creativecommons.org/ns#\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" width=\"210mm\" height=\"297mm\" viewBox=\"0 0 744.09448819 1052.3622047\" id=\"svg2\" version=\"1.1\" inkscape:version=\"0.91 r13725\" sodipodi:docname=\"template_pages.svg\"> <defs id=\"defs4\" /> <sodipodi:namedview id=\"base\" pagecolor=\"#ffffff\" bordercolor=\"#666666\" borderopacity=\"1.0\" inkscape:pageopacity=\"0.0\" inkscape:pageshadow=\"2\" inkscape:zoom=\"0.7\" inkscape:cx=\"305.27455\" inkscape:cy=\"725.4968\" inkscape:document-units=\"px\" inkscape:current-layer=\"layer3\" showgrid=\"false\" inkscape:window-width=\"1280\" inkscape:window-height=\"751\" inkscape:window-x=\"0\" inkscape:window-y=\"0\" inkscape:window-maximized=\"1\" /> <metadata id=\"metadata7\"> <rdf:RDF> <cc:Work rdf:about=\"\"> <dc:format>image/svg+xml</dc:format> <dc:type rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" /> <dc:title /> </cc:Work> </rdf:RDF> </metadata> <g inkscape:label=\"svgpages___layer0\" inkscape:groupmode=\"layer\" id=\"layer1\" style=\"display:none\"> <g id=\"g4165\"> <rect y=\"133.12338\" x=\"62.629459\" height=\"189.90868\" width=\"242.43661\" id=\"rect4136\" style=\"opacity:1;fill:none;fill-opacity:1;stroke:#000000;stroke-width:1;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1\" /> <text sodipodi:linespacing=\"125%\" id=\"text4144\" y=\"292.36221\" x=\"177.14285\" style=\"font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:40px;line-height:125%;font-family:sans-serif;-inkscape-font-specification:&#39;sans-serif, Normal&#39;;text-align:center;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" xml:space=\"preserve\"><tspan y=\"292.36221\" x=\"177.14285\" id=\"tspan4146\" sodipodi:role=\"line\">Layer0</tspan></text> </g> <text xml:space=\"preserve\" style=\"font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:40px;line-height:125%;font-family:sans-serif;-inkscape-font-specification:&#39;sans-serif, Normal&#39;;text-align:center;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" x=\"107.14286\" y=\"353.79077\" id=\"text4227\" sodipodi:linespacing=\"125%\"><tspan sodipodi:role=\"line\" x=\"107.14286\" y=\"353.79077\" id=\"tspan4231\">BD Analog Blip</tspan></text> </g> </svg>" of 
    --    xml -> do

    readFile template >>= \str -> do
        
        let xml = parseXML (templatemapSVG' tm str)
        -- (___xxx words are now gone)

        let path0 = dir </> (takeBaseName kit ++ "_page0") <.> "svg"
            path1 = dir </> (takeBaseName kit ++ "_page1") <.> "svg"
            path2 = dir </> (takeBaseName kit ++ "_page2") <.> "svg"

        -- write 3 files by modifying the root element
        writeVisibleLayer path0 "svgpages___layer0" xml 
        writeVisibleLayer path1 "svgpages___layer1" xml 
        writeVisibleLayer path2 "svgpages___layer2" xml 
        
        return $ Just $ SVGPages (tmName tm) path0 path1 path2

    where
      writeVisibleLayer path label conts = do
          writeFile path $ concat $ map showContent $ visibly label conts

      visibly :: String -> [Content] -> [Content]
      visibly label =
          mapElements (\elem -> if svgElementHasLabel label elem then elementSetAttribute (toQName "style") "display:inline" elem else elem)

      myElem elem =
          elem { elName = unqual "MyElem", elContent = [], elAttribs = [] }
          
                          
--------------------------------------------------------------------------------
--  SVG

svgElementHasLabel :: String -> Element -> Bool
svgElementHasLabel = 
    elementAttributeIs (toQName "inkscape:label")

