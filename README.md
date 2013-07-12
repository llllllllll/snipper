snipper
=======
A command line utility for saving snippets of code or notes by Joe Jevnik
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
 help - Prints this message."