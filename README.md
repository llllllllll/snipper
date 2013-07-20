snipper
=======
-----------------------------------------------------------------------------
-- Snipper v1.0 (release)
-- Description: A command line utility for saving code snippets or notes.
-- Author: Joe Jevnik
-- Portability: Requires POSIX
-- Dependencies: xclip for "clip" and "copy" commands
-- Copyright Joe Jevnik 2013

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-----------------------------------------------------------------------------

========
Commands:
=========
Commands:
 add <title> <lang> <cont> - adds a Snip with the given 
                             parameters
 print <title> - Returns the Snip with the given title.
 search <fragment> - Returns the title of Snips that
                     have fragment anywhere in their
                     contents.
 lang <lang> - Returns a list of all Snips that are of
               the given language.
 list - Returns the title of every Snip in your library.
 remove <title> - Removes the give Snip from the library.
 clear - Clears your whole library.
 copy <title> - copies the contents of the snip to the clipoard.
 clip <title> <lang> - Creates a snip with the given title and 
                       lang and pulls the contents from the clipboard.
 eval <title> - Evaluates the Snip if it is a valid function
 version - Returns the given version information.
 help - Prints this message.
