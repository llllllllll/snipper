-----------------------------------------------------------------------------
-- Snipper v1.1
-- Description: A command line utility for saving code snippets or notes.
-- Author: Joe Jevnik
-- Dependencies: xclip for "clip" and "cp" commands
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

import System.IO
import System.Environment
import System.Directory
import System.Process
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA

-- Snip library,
io_dot_snips :: IO FilePath
io_dot_snips = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/.snips"

-- Temp Snip libary for editing.
io_dot_snips' :: IO FilePath
io_dot_snips' = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips'"

-- Temp haskell source file for eval haskell.
io_temp_hs :: IO FilePath
io_temp_hs = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips.hs"

-- Temp object file for eval haskell.
io_temp_o :: IO FilePath
io_temp_o = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips.o"

-- Temp interface file for eval haskell.
io_temp_hi :: IO FilePath
io_temp_hi = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips.hi"

-- Temp c source file for eval c/c++
io_temp_c :: IO FilePath
io_temp_c = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips.c"

-- Temp binary file for eval.
io_temp_proc :: IO FilePath
io_temp_proc = do
    h <- getHomeDirectory
    return $ h ++ "/.snipper/#.snips"

-- A Snippet of code or text.
data Snip = Snip { title :: String
                 , language :: String
                 , contents :: String
                 } deriving (Eq)
            
-- The output for print, and is modified for saveing.
instance Show Snip where
    show s = "Title: " ++ title s ++ "\nLang:  " ++ language s ++ "\nCont:  " 
             ++ contents s

main :: IO ()
main = do
    args <- getArgs
    if (null args) 
      then putStrLn "Usage: Snipper COMMAND (PARAM)" 
      else apply args

-- Formats a Snip into a string suitable for being saved in the dot_snips file.
save_format :: Snip -> String
save_format s = (intercalate "¶" (splitOn "\n" (show s))) ++ "\n"

-- Parses the command line args.
apply :: [String] -> IO ()
apply args
    | head args `elem` 
      ["print","search","lang","rm","cp","eval","regex","searchl","searcht"] = 
          parse_command (head args) $ Snip (args !! 1) "" ""
    | head args `elem` ["mk","edit"] = let s = tail args in
                                       parse_command (head args) $ 
                                       Snip (head s) (s !! 1) (s !! 2) 
    | head args `elem` ["count","ls","cl","help","version"] = 
          parse_command (head args) $ Snip "" "" "" 
    | head args == "clip" = parse_command (head args) $ 
                            Snip (args !! 1) (args !! 2) ""
    | otherwise = putStrLn "Error: Command not recognized."

-- Parses a String to a command function.
parse_command :: String -> (Snip -> IO ())
parse_command str
    | str == "mk"      = add_snip
    | str == "print"   = print_snip
    | str == "cont"    = search_cont
    | str == "search"  = regex_cont
    | str == "searcht" = regex_title
    | str == "lang"    = regex_lang
    | str == "searchl" = search_lang
    | str == "count"   = count_snips
    | str == "ls"      = list_snips
    | str == "rm"      = remove_snip
    | str == "cl"      = clear_snips
    | str == "cp"      = copy_snip
    | str == "eval"    = eval_snip
    | str == "clip"    = from_clip
    | str == "edit"    = edit_snip
    | str == "help"    = snipper_help
    | str == "version" = snipper_version

-- Parses the .snips into a list of Snips
parse_snips :: String -> [Snip]
parse_snips str = map mk_snip (lines str)
  where
      mk_snip str = let fields = splitOn "¶" str in
                    Snip { title    = fromMaybe "" 
                                      (stripPrefix "Title: " (head fields))
                         , language = fromMaybe "" 
                                      (stripPrefix "Lang:  " (fields !! 1)) 
                         , contents = fromMaybe "" (stripPrefix "Cont:  " 
                                                    (concat $ intersperse "\n" 
                                                    $ tail $ tail fields))
                         }

snipper_help :: Snip -> IO ()
snipper_help _ = 
    putStrLn "COMMANDS:\nmk <title> <lang> <cont> - Adds a Snip with the given parameters.\nclip <title> <lang> - Works like mk, but <cont> is taken from\n                      the clipboard.\nrm <title> - Removes the given Snip.\nedit <title> <lang> <cont> - Edits the given Snip to \n                             have the new parameters.\nprint <title> - Prints the given Snip.\ncp <title> - Copies the contents of the given Snip to\n             the clipboard.\nsearch <cont>  - Prints a list of Snip titles that have <cont>\n                 somewhere in their contents.\nlang <lang> - Prints a list of Snip titles that are of\n              language <lang>\nls - Prints the title of all Snips you have saved.\neval <title> - Attempts to evaluate the given snip if it is a\n               parameterless function. Currently only supports\n               haskell and c/c++. WARNING: this feature is buggy.\ncl - Resets your Snip library.\nhelp - Prints this message.\nversion - Prints the version information."

snipper_version :: Snip -> IO ()
snipper_version _ = putStrLn "Snipper by Joe Jevnik\nVersion: v1.1"

-- The function to add a Snip to .snips
add_snip :: Snip -> IO ()
add_snip s = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    if any (==title s) (map title snips) 
      then putStrLn $ title s ++ 
               " is already in use!\nUse \"snipper remove " ++ title s 
               ++ "\" to free the name."
      else appendFile dot_snips (save_format s)
  
-- Prints a Snip to stdout.
print_snip :: Snip -> IO ()
print_snip s = do 
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    putStrLn $ show $ fromMaybe (Snip "Snip Not Found" "" "") $ 
               find (\sn -> title s == title sn) snips

-- DEPRECATED: Use regex_cont
-- Prints the titles of Snips that have contents that conatain the title s.
search_cont :: Snip -> IO ()
search_cont s = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    let str = concatMap (\sn -> title sn ++ "\n") $ 
                filter (\sn -> title s `isInfixOf` contents sn) snips
    if null str 
      then putStrLn "No Snips match your search!"
      else putStr str

-- Prints the titles of the Snips that have contents that match regex title s.
regex_cont :: Snip -> IO ()
regex_cont s = do
    dot_snips <- io_dot_snips
    snips <- parse_snips <$> readFile dot_snips
    let str = concatMap (\sn -> title sn ++ "\n") $ 
              filter (\sn -> contents sn =~ title s :: Bool) snips 
    if null str
      then putStrLn "No Snips match your search!"
      else putStr str

-- DEPRECATED: Use regex_lang
-- Prints the titles of Snips that are of language lang s
search_lang :: Snip -> IO ()
search_lang s = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    let str = concatMap (\sn -> title sn ++ "\n") $ 
              filter (\sn -> language sn == title s) snips 
    if null str
      then putStrLn "No Snips match your search!"
      else putStr str

-- Prints the titles of the Snips that have lang that match regex title s.
regex_lang :: Snip -> IO ()
regex_lang s = do
    dot_snips <- io_dot_snips
    snips <- parse_snips <$> readFile dot_snips
    let str = concatMap (\sn -> title sn ++ "\n") $ 
              filter (\sn -> language sn =~ title s :: Bool) snips 
    if null str
      then putStrLn "No Snips match your search!"
      else putStr str
  
-- Prints the number of Snips you have saved.
count_snips :: Snip -> IO ()
count_snips _ = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    print $ length snips

-- Prints the titles of all the Snips
list_snips :: Snip -> IO ()
list_snips _ = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    let str = concatMap (\s -> title s ++ "\n") snips
    if null str
      then putStrLn "No Snips found!"
      else putStr str

-- Prints the titles of the Snips that have titles that match regex title s.
regex_title :: Snip -> IO ()
regex_title s = do
    dot_snips <- io_dot_snips
    snips <- parse_snips <$> readFile dot_snips
    let str = concatMap (\sn -> title sn ++ "\n") $ 
              filter (\sn -> title sn =~ title s :: Bool) snips 
    if null str
       then putStrLn "No Snips match your search!"
       else putStr str

-- Removes a Snip from the .snips.
remove_snip :: Snip -> IO ()
remove_snip s =  do 
    dot_snips <- io_dot_snips
    snips_handle <- openFile dot_snips ReadWriteMode
    snips <- liftM parse_snips $ hGetContents snips_handle
    if any (==title s) (map title snips) 
      then remove_snip snips_handle snips >>
                 (putStrLn $ title s ++ " removed!")
      else putStrLn $ title s ++ " is not already in use"
  where
      remove_snip snips_handle snips = do
                     dot_snips' <- io_dot_snips'
                     dot_snips <- io_dot_snips
                     snips_handle' <- openFile dot_snips' ReadWriteMode     
                     mapM (hPutStr snips_handle' . save_format) 
                              (filter (\sn -> title sn /= title s) snips)
                     mapM hClose [snips_handle, snips_handle']
                     removeFile dot_snips
                     renameFile dot_snips' dot_snips
        
-- Puts the contents of s into the clipboard.
copy_snip :: Snip -> IO ()
copy_snip s = do 
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    system $ "echo " ++ (contents $ fromMaybe (Snip "Snip Not Found" "" "") $
                         find (\sn -> title s == title sn) snips) 
               ++ " | xclip -selection c"
    return ()
        
-- Adds a Snip with the contents pulled from the clipboard.
from_clip :: Snip -> IO ()
from_clip s = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    snip_cont <- readProcess "xclip" ["-o"] []
    if any (==title s) (map title snips) 
      then putStrLn $ title s 
               ++ " is already in use (use rm to clear the title)"
      else 
          appendFile dot_snips $ save_format $ 
              Snip (title s) (language s) (snip_cont)

-- Resets your .snips file.
clear_snips :: Snip -> IO ()
clear_snips _ = do
    dot_snips <- io_dot_snips
    orig_b <- hGetBuffering stdin
    hSetBuffering stdin LineBuffering
    putStr "Are you sure you want to clear your Snips library? (y/N):"
    hFlush stdout
    inp <- getChar
    hSetBuffering stdin orig_b
    when (inp == 'y') $ openFile dot_snips WriteMode >>= hClose
  
-- Edits a Snip to match s or adds a new one.
edit_snip :: Snip -> IO ()
edit_snip s = do
    dot_snips <- io_dot_snips
    snips_handle <- openFile dot_snips ReadWriteMode
    snips <- liftM parse_snips $ hGetContents snips_handle
    if any (==title s) (map title snips) 
      then remove_snip snips_handle snips >> add_snip s
      else add_snip s
  where
      remove_snip snips_handle snips = do
                  dot_snips' <- io_dot_snips'
                  dot_snips <- io_dot_snips
                  snips_handle' <- openFile dot_snips' ReadWriteMode     
                  mapM (hPutStr snips_handle' . save_format) 
                           (filter (\sn -> title sn /= title s) snips)
                  mapM hClose [snips_handle, snips_handle']
                  removeFile dot_snips
                  renameFile dot_snips' dot_snips
  
-- Attempts to compile and run the given snip.
eval_snip :: Snip -> IO ()
eval_snip s = do
    dot_snips <- io_dot_snips
    snips <- liftM parse_snips $ readFile dot_snips
    eval_snip' $ fromMaybe (Snip "Snip Not Found" "" "") $ 
               find (\sn -> title s == title sn) snips
  where
      eval_snip' sn
          | title sn == "Snip Not Found"             = putStrLn "Snip Not Found"
          | language sn `elem` ["haskell","Haskell"] = eval_haskell sn
          | language sn `elem` ["c","C","c++","C++"] = eval_c sn
          | otherwise = putStrLn "Language not yet supported."

-- Imports Data.List
eval_haskell :: Snip -> IO ()
eval_haskell s = do
    temp_hs <- io_temp_hs
    temp_hi <- io_temp_hi
    temp_o <- io_temp_o
    temp_proc <- io_temp_proc
    h <- openFile temp_hs WriteMode
    let fixed_conts = let cs = splitOn "=" (contents s) 
                      in "module Main where\nimport Data.List\n" 
                             ++ head cs ++  "= print $ " ++ (concat $ tail cs)
    hPutStrLn h (fixed_conts)
    hClose h
    let func = takeWhile (/=' ') (contents s)
    system $ "ghc -main-is " ++ func ++ " --make " ++ temp_hs
    readProcess temp_proc [] [] >>= putStr
    mapM_ removeFile [temp_hs,temp_hi,temp_o,temp_proc] >> putStr ""

-- Includes stdio.h
eval_c :: Snip -> IO ()
eval_c s = do
    temp_c <- io_temp_c
    temp_proc <- io_temp_proc
    h <- openFile temp_c WriteMode
    let func = dropWhile (/=' ') $ takeWhile (/='{') (contents s)
        fixed_conts = let cs = splitOn "=" (contents s) 
                      in "#include<stdio.h>\n\nint main(){ printf(\"%i\"," 
            ++ func ++ ");\nprintf(\"\\n\");\nreturn 0; }\n\n" ++ contents s  
    hPutStrLn h (fixed_conts)
    hClose h
    system $ "gcc " ++ temp_c ++ " -o" ++ temp_proc ++ " -lm"
    readProcess temp_proc [] [] >>= putStr
    mapM_ removeFile [temp_c,temp_proc] >> putStr ""
