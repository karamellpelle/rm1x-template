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
module XML 
  (
    modifyElementIf,
    mapElements,
    elementSetAttribute,
    elementHasAttribute,
    elementAttributeIs,
    toQName,

    module Text.XML.Light,

  ) where

import Text.XML.Light
import Helpers


-- | modify the first occurence
modifyElementIf :: (Element -> Bool) -> (Element -> Element) -> [Content] -> [Content]
modifyElementIf pred f = \conts -> 
    map helper conts
    where
      helper = \cont -> case cont of
        (Text _)  -> cont
        (CRef _)  -> cont
        (Elem elem@(Element name attr conts line )) -> if pred elem 
            then Elem $ f elem                                                 -- no more searching; return
            else Elem $ Element name attr (modifyElementIf pred f conts) line  -- continue search

-- | modify. foldr
mapElements :: (Element -> Element) -> [Content] -> [Content]
mapElements f = \conts -> 
    map helper conts
    where
      helper = \cont -> case cont of
        (Text _)    -> cont
        (CRef _)    -> cont
        (Elem elem) -> 
            let elem' = f elem
            in  Elem $ elem' { elContent = mapElements f $ elContent elem' } -- continue replacement



elementSetAttribute :: QName -> String -> Element -> Element
elementSetAttribute qname value = \elem -> case elem of
    (Element name attrs conts line) -> 
        Element name (setoradd attrs) conts line

    where
      setoradd []     = Attr qname value : []
      setoradd (a:as) = if qnameEqual' (attrKey a) qname 
                        then a { attrVal = value } : as
                        else a : setoradd as


elementHasAttribute :: QName -> Element -> Bool
elementHasAttribute qname = \(Element name attrs conts line) -> 
    isJust $ lookupAttr' qname attrs
      
        

elementAttributeIs :: QName -> String -> Element -> Bool
elementAttributeIs qname value = \(Element name attrs conts line) -> 
    case lookupAttr' qname attrs of 
        Nothing   -> False
        Just  str -> str == value


toQName :: String -> QName
toQName str = 
    case span (/= ':') str of
        (str', [])    -> QName str' Nothing Nothing
        (str', str'') -> QName (tail str'') Nothing (Just str')



--------------------------------------------------------------------------------
-- since we only care about name + prefix

lookupAttr' :: QName -> [Attr] -> Maybe String
lookupAttr' qname = \attrs ->
    case attrs of 
        []      -> Nothing
        (a:as)  -> if qnameEqual' qname $ attrKey a
                   then Just $ attrVal a
                   else lookupAttr' qname as


qnameEqual' :: QName -> QName -> Bool
qnameEqual' (QName name _ prefix) (QName name' _ prefix') =
    name' == name && prefix' == prefix

