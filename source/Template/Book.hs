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
module Template.Book
  (
      Book (..),
      makeBook
  ) where

import Helpers
import Template.Map
import Template.File
import Template.SVGPages


data Book =
    Book
    {
        bookName :: String,
        bookPath :: FilePath
    }



-- | make a pdf with LaTex from kit files. returns 'out' upon
--   success
makeBook :: [FilePath] -> FilePath -> IO (Maybe Book)
makeBook kits out = do
    withSystemTempDirectory "rm1x-template" $ \dir -> do
        
        putStrLn $ "temporary directory: " ++ dir


        -- create images
        svgs <- fmap catMaybes $ forM kits $ \kit -> makeSVGPages kit dir

        -- create includes from svgs
        latexs <- forM svgs $ makeLatexInclude dir


        let mainTex = dir </> "main.tex"
        header <- readFile =<< getDataFileName' "header.tex"
        footer <- readFile =<< getDataFileName' "footer.tex"

        writeFile mainTex $ 
            header ++
            concatMap (\path -> "\\include{" ++ path ++ "}\n") latexs ++
            footer
        

        compileLaTeX mainTex out

        return $ Just $ Book "Yamaha Rm1X" out


-- | create latex code from 3 svg images, for inclution in main latex file
makeLatexInclude :: FilePath -> SVGPages -> IO FilePath
makeLatexInclude dir (SVGPages name svg0 svg1 svg2) = do

    let include = dir </> name <.> ".tex"

    template <- getDataFileName' "include.tex"

    -- use template to generate a new include file
    -- write include file (replace words)
    readFile template >>= \str -> do
        writeFile include $ templatemap (TemplateMap name sm) str

    return include

    where 
      sm = [ ("___NAME", name),
             ("___SVG0", svg0), 
             ("___SVG1", svg1), 
             ("___SVG2", svg2)
             ]


compileLaTeX :: FilePath -> FilePath -> IO (Maybe FilePath)
compileLaTeX input output = do
    res <- system $ "cd " ++ (takeDirectory input)            ++ " && " ++
                    "latex " ++ input  ++ " -o "    ++ output ++ " && " ++
                    "latex " ++ input  ++ " -o "    ++ output              -- lets do it one more time to get references
    
    case res of 
        ExitSuccess   -> return $ Just output
        ExitFailure _ -> return Nothing

