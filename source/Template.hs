import Data.List
import Data.String


midiMap :: FilePath -> IO [(String, String)]
midiMap path =
    readFile path >>= \file ->
        return $ makeMap $ lines file
        where
            makeMap ls = foldr step [] ls
            step line xs =
                case words line of 
                    (w0:ws@(w1:ws2)) -> (w0, unwords ws) : xs   -- line has at least 2 words
                    _                -> xs                      -- do nothing and keep rollin'
