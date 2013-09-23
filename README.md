snipper
=======
Module      :  Snipper
Copyright   :  Joe Jevnik 2013
License     :  GPL v3

Maintainer  :  Joe Jevnik
Stability   :  experimental
Portability :  requires xclip

A command line utility for saving, searching for, and recalling
snippets of code or text.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/".

Commands:
--------

-  mk "title" "lang" "cont" - Adds a Snip with the given parameters.
-  clip "title" "lang" - Works like mk, but "cont" is taken from
                        the clipboard.
-  rm "title" - Removes the given Snip.
-  edit "title" "lang" "cont" - Edits the given Snip to
                               have the new parameters.
-  print "title" - Prints the given Snip.
-  cp "title" - Copies the contents of the given Snip to
               the clipboard.
-  search "cont"  - Prints a list of Snip titles that have "cont"
                   somewhere in their contents.
-  lang "lang" - Prints a list of Snip titles that are of
                language "lang"
-  ls - Prints the title of all Snips you have saved.
-  eval "title" - Attempts to evaluate the given snip if it is a
                 parameterless function. Currently only supports
                 haskell and c/c++. WARNING: this feature is deprecated.
-  cl - Resets your Snip library.\nhelp - Prints this message.
-  version - Prints the version information.

TODO:
----

- Add an edit from clipboard
- Fix readme
- Add more features as I see fit
