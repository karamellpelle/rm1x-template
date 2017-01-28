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
    
        putStrLn "Creating book:"
        

        -- create images
        svgs <- fmap catMaybes $ forM kits $ \kit -> makeSVGPages kit dir

        -- create includes from svgs
        latexs <- forM svgs $ makeLatexInclude dir

        let mainTex = dir </> "main.tex"
        header <- readFile =<< getDataFileName' "header.tex"
        footer <- readFile =<< getDataFileName' "footer.tex"

        writeFile mainTex $ 
            (templatemap tmLatex header)                               ++
            concatMap (\path -> "\\input{\"" ++ path ++ "\"}\n") latexs ++
            "\n"                                                        ++
            (templatemap tmLatex footer)

        --putStrLn ("tmp dir: " ++ dir) >> getLine

        fmap (fmap (\pdf -> Book "Yamaha Rm1x" pdf)) $ compileLaTeX mainTex out
        -- ^ TODO: copy log upon error?



    where
      tmLatex = TemplateMap "" [ 
                ("___VERSION", version')
                ]
      version' = showVersion version ++ "-" ++ gitHEADShort


-- | create latex code from 3 svg images, for inclution in main latex file
makeLatexInclude :: FilePath -> SVGPages -> IO FilePath
makeLatexInclude dir (SVGPages name svg0 svg1 svg2) = do

    putStrLn $ "Generating include file for " ++ name ++ " ..."

    -- latex don't like svgs :(
    -- let's use inkscape to convert to .pdf files (images), and then
    -- include these new files in the latex document!
    --
    -- inkscape can be installed with brew: `brew cask install inkscape`
    -- (a package downloaded from inkscape.org may not add its command line
    -- commands to PATH).
    --
    -- NOTE: there is a bug with my inkscape (0.91) that only let it use
    --       absolute path. since 'dir' and svg's are absolute paths (as returned
    --       from withSystemTempDirectory, this works.
    --
    [pdf0, pdf1, pdf2] <- forM [svg0, svg1, svg2] $ \svg -> do
        let pdf = dropExtension svg ++ ".pdf"
        system $ "inkscape -f " ++ svg ++ " -A " ++ pdf
        return pdf

    template <- getDataFileName' "include.tex"

    -- use template to generate a new include file
    -- write include file (replace words)
    let include = dir </> name <.> ".tex"
    readFile template >>= \str -> do
        writeFile include $ templatemap (tmLatex pdf0 pdf1 pdf2) str

    return include

    where 
      tmLatex p0 p1 p2 = TemplateMap name [
                        ("___NAME", name),
                        ("___IMAGE0", p0), 
                        ("___IMAGE1", p1), 
                        ("___IMAGE2", p2)
                        ]


compileLaTeX :: FilePath -> FilePath -> IO (Maybe FilePath)
compileLaTeX tex pdf = do

    putStrLn "Compiling LaTeX ..."

    res <- system $ "cd " ++ (takeDirectory tex)        ++ " && " ++
                    "pdflatex -interaction=nonstopmode " ++ tex ++ " > log.txt"  ++ " && " ++
                    "pdflatex -interaction=nonstopmode " ++ tex ++ " > log.txt"  -- lets do it one more time to get references
    
    case res of 
        ExitSuccess   -> do
            -- since latex is a ugly command line program, we have to
            -- move the generated pdf from temporary folder
            copyFile (dropExtension tex ++ ".pdf") pdf
            return $ Just pdf

        ExitFailure _ -> return Nothing

