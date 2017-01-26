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
module Git
  (
    gitHEAD,
    gitHEADShort,
  ) where


--------------------------------------------------------------------------------
-- these can be updated with the 'git2hs.sh' command

-- | full commit hash of HEAD
gitHEAD :: String
gitHEAD = "RM1X"

-- | shortened commit hash of HEAD
gitHEADShort :: String
gitHEADShort = "rm1x"
