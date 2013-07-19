-----------------------------------------------------------------------------
-- Snipper v0.5
-- Author: Joe Jevnik
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
import Data.Functor
import Control.Applicative
import Control.Monad

io_dot_snips :: IO FilePath
io_dot_snips = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/.snips"
  

io_dot_snips' :: IO FilePath
io_dot_snips' = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips'"

io_temp_hs :: IO FilePath
io_temp_hs = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips.hs"

io_temp_o :: IO FilePath
io_temp_o = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips.o"

io_temp_hi :: IO FilePath
io_temp_hi = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips.hi"

io_temp_c :: IO FilePath
io_temp_c = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips.c"

io_temp_proc :: IO FilePath
io_temp_proc = do
  h <- getHomeDirectory
  return $ h ++ "/.snipper/#.snips"

-- A Snippet of code.
data Snip = Snip { title :: String
                 , language :: String
                 , contents :: String
                 } deriving (Eq)
            
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
apply args
  | head args `elem` ["print","search","lang","remove","copy","eval"] = 
    parse_command (head args) $ Snip (args !! 1) "" ""
  | head args == "add"  = let s = tail args in
  add_snip $ Snip (head s) (s !! 1) (s !! 2) 
  | head args `elem` ["count","list","clear","help","version"] = 
    parse_command (head args) $ Snip "" "" "" 
  | head args == "clip" = parse_command (head args) $ Snip (args !! 1) (args !! 2) ""
  | otherwise = putStrLn "Error: Command not recognized."

-- Parses a String to a command function.
parse_command :: String -> (Snip -> IO ())
parse_command str
  | str == "add"     = add_snip
  | str == "print"   = print_snip
  | str == "search"  = search_cont
  | str == "lang"    = search_lang
  | str == "count"   = count_snips
  | str == "list"    = list_snips
  | str == "remove"  = remove_snip
  | str == "clear"   = clear_snips
  | str == "copy"    = copy_snip
  | str == "eval"    = eval_snip
  | str == "clip"    = from_clip
  | str == "help"    = snipper_help
  | str == "version" = snipper_version

-- Parses the .snips into a list of Snips
parse_snips :: String -> [Snip]
parse_snips str = map mk_snip (lines str)
  where
    mk_snip str = let fields = splitOn "¶" str in
      Snip { title    = fromMaybe "" (stripPrefix "Title: " (head fields))
           , language = fromMaybe "" (stripPrefix "Lang:  " (fields !! 1)) 
           , contents = fromMaybe "" (stripPrefix "Cont:  " 
                                      (concat $ intersperse "\n" 
                                       $  tail $ tail fields))
           }

snipper_help :: Snip -> IO ()
snipper_help _ = 
  putStrLn "Commands:\n add <title> <lang> <cont> - adds a Snip with the given \n                             parameters\n print <title> - Returns the Snip with the given title.\n search <fragment> - Returns the title of Snips that\n                     have fragment anywhere in their\n                     contents.\n lang <lang> - Returns a list of all Snips that are of\n               the given language.\n list - Returns the title of every Snip in your library.\n remove <title> - Removes the give Snip from the library.\n clear - Clears your whole library.\n copy <title> - copies the contents of the snip to the clipoard.\n clip <title> <lang> - Creates a snip with the given title and \n                       lang and pulls the contents from the clipboard.\n eval <title> - Evaluates the Snip if it is a valid function\n version - Returns the given version information.\n help - Prints this message."

snipper_version :: Snip -> IO ()
snipper_version _ = putStrLn "Snipper by Joe Jevnik\nVersion: 0.5"


-- The function to add a Snip to .snips
add_snip :: Snip -> IO ()
add_snip s = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  if any (==title s) (map title snips) 
    then do  
    putStrLn $ title s ++ " is already in use, use \"snipper remove " ++ title s 
      ++ "\" to free the name"
    else 
     appendFile dot_snips $ save_format s

-- Prints a Snip to stdout.
print_snip :: Snip -> IO ()
print_snip s = do 
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  putStrLn $ show $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips


-- Prints the titles of snips that have contents that conatain the title s.
search_cont :: Snip -> IO ()
search_cont s = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\sn -> title sn ++ "\n") $ 
    filter (\sn -> title s `isInfixOf` contents sn) snips

search_lang :: Snip -> IO ()
search_lang s = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\sn -> title sn ++ "\n") $ 
    filter (\sn -> language sn == title s) snips 

-- Prints the number of Snips you have saved.
count_snips :: Snip -> IO ()
count_snips _ = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  print $ length snips

list_snips :: Snip -> IO ()
list_snips _ = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\s -> title s ++ "\n") snips

-- Removes a Snip from the .snips.
remove_snip :: Snip -> IO ()
remove_snip s =  do 
  dot_snips <- io_dot_snips
  snips_handle <- openFile dot_snips ReadWriteMode
  snips <- liftM parse_snips $ hGetContents snips_handle
  if any (==title s) (map title snips) 
    then remove_snip snips_handle snips
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
        
copy_snip :: Snip -> IO ()
copy_snip s = do 
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  system $ "echo \"" ++ (contents $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips) ++ "\" | xclip -selection c" 
  putStrLn "Copied!"
        
from_clip :: Snip -> IO ()
from_clip s = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  snip_cont <- readProcess "xclip" ["-o"] []
  if any (==title s) (map title snips) 
    then putStrLn $ title s ++ " is already in use (use remove to clear the title)"
    else 
     appendFile dot_snips $ save_format $ Snip (title s) (language s) (snip_cont)

clear_snips :: Snip -> IO ()
clear_snips _ = do
  dot_snips <- io_dot_snips
  orig_b <- hGetBuffering stdin
  hSetBuffering stdin LineBuffering
  putStrLn "Are you sure you want to clear your Snips library? (y/n):"
  inp <- getChar
  hSetBuffering stdin orig_b
  when (inp == 'y') $ openFile dot_snips WriteMode >>= hClose
  

eval_snip :: Snip -> IO ()
eval_snip s = do
  dot_snips <- io_dot_snips
  snips <- liftM parse_snips $ readFile dot_snips
  exec_snip' $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips
  where
    exec_snip' sn
      | title sn == "Snip Not Found" = putStrLn "Snip Not Found"
      | language sn == "haskell"     = eval_haskell sn
      | language sn == "c"           = eval_c sn
      | otherwise = putStrLn "Language not yet supported."

eval_haskell :: Snip -> IO ()
eval_haskell s = do
  temp_hs <- io_temp_hs
  temp_hi <- io_temp_hi
  temp_o <- io_temp_o
  temp_proc <- io_temp_proc
  h <- openFile temp_hs WriteMode
  let fixed_conts = 
        let cs = splitOn "=" (contents s) 
        in "module Main where\nimport Data.List\n" 
           ++ head cs ++  "= print $ " ++ (concat $ tail cs)
  hPutStrLn h (fixed_conts)
  hClose h
  let func = takeWhile (/=' ') (contents s)
  system $ "ghc -main-is " ++ func ++ " --make " ++ temp_hs
  readProcess temp_proc [] [] >>= putStr
  mapM_ removeFile [temp_hs,temp_hi,temp_o,temp_proc] >> putStr ""
  
eval_c :: Snip -> IO ()
eval_c s = do
  temp_c <- io_temp_c
  temp_proc <- io_temp_proc
  h <- openFile temp_c WriteMode
  let 
    func = dropWhile (/=' ') $ takeWhile (/='{') (contents s)
    fixed_conts = 
        let cs = splitOn "=" (contents s) 
        in "#include<stdio.h>\n\nint main(){ printf(\"%i\"," 
           ++ func ++ ");\nreturn 0; }\n\n" ++ contents s  
  hPutStrLn h (fixed_conts)
  hClose h
  system $ "gcc " ++ temp_c ++ " -o" ++ temp_proc ++ " -lm"
  readProcess temp_proc [] [] >>= putStr
  mapM_ removeFile [temp_c,temp_proc] >> putStr ""