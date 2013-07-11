import System.IO
import System.Environment
import System.Directory
import System.Cmd
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

dot_snips :: FilePath
dot_snips = "/home/joejev/.snips"

dot_snips' :: FilePath
dot_snips' = "/home/joejev/#.snips"

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
save_format s = (intercalate "\\" (splitOn "\n" (show s))) ++ "\n"

-- Parses the command line args.
apply args
  | head args `elem` ["print","search","lang","remove","copy"] = parse_command 
                                                                 (head args) $ 
                                                                 Snip (args !! 1) "" ""
  | head args == "add"  = let s = tail args in
  add_snip $ Snip (head s) (s !! 1) (s !! 2) 
  | head args `elem` ["count","list","clear","help","version"] = 
    parse_command (head args) $ Snip "" "" "" 
  | otherwise = putStrLn "Error: Command not regognized."

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
  | str == "help"    = snipper_help
  | str == "version" = snipper_version

-- Parses the .snips into a list of Snips
parse_snips :: String -> [Snip]
parse_snips str = map mk_snip (lines str)
  where
    mk_snip str = let fields = splitOn "\\" str in
      Snip { title    = fromMaybe "" (stripPrefix "Title: " (head fields))
           , language = fromMaybe "" (stripPrefix "Lang:  " (fields !! 1)) 
           , contents = fromMaybe "" (stripPrefix "Cont:  " (fields !! 2))
           }

snipper_help :: Snip -> IO ()
snipper_help _ = 
  putStrLn "Commands:\n add <title> <lang> <cont> - adds a Snip with the given \n                             parameters\n print <title> - Returns the Snip with the given title.\n search <fragment> - Returns the title of Snips that\n                     have fragment anywhere in their\n                     contents.\n lang <lang> - Returns a list of all Snips that are of\n               the given language.\n list - Returns the title of every Snip in your library.\n remove <title> - Removes the give Snip from the library.\n clear - Clears your whole library.\n copy <title> - copies the contents of the snip to the clipoard.\n version - Returns the given version information.\n help - Returns this message."

snipper_version :: Snip -> IO ()
snipper_version _ = putStrLn "Snipper by Joe Jevnik\nVersion: 0.2"


-- The function to add a Snip to .snips
add_snip :: Snip -> IO ()
add_snip s = do
  snips <- liftM parse_snips $ readFile dot_snips
  if any (==title s) (map title snips) 
    then putStrLn $ title s ++ " is already in use (use remove to clear the title)"
    else 
     appendFile dot_snips $ save_format s

-- Prints a Snip to stdout.
print_snip :: Snip -> IO ()
print_snip s = do 
  snips <- liftM parse_snips $ readFile dot_snips
  putStrLn $ show $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips


-- Prints the titles of snips that have contents that conatain the title s.
search_cont :: Snip -> IO ()
search_cont s = do
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\sn -> title sn ++ "\n") $ 
    filter (\sn -> title s `isInfixOf` contents sn) snips

search_lang :: Snip -> IO ()
search_lang s = do
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\sn -> title sn ++ "\n") $ 
    filter (\sn -> language sn == title s) snips 

-- Prints the number of Snips you have saved.
count_snips :: Snip -> IO ()
count_snips _ = do
  snips <- liftM parse_snips $ readFile dot_snips
  print $ length snips

list_snips :: Snip -> IO ()
list_snips _ = do
  snips <- liftM parse_snips $ readFile dot_snips
  putStr $ concatMap (\s -> title s ++ "\n") snips

-- Removes a Snip from the .snips.
remove_snip :: Snip -> IO ()
remove_snip s =  do 
  snips_handle <- openFile dot_snips ReadWriteMode
  snips <- liftM parse_snips $ hGetContents snips_handle
  if any (==title s) (map title snips) 
    then remove_snip snips_handle snips
    else putStrLn $ title s ++ " is not already in use"
    where
      remove_snip snips_handle snips = do
        snips_handle' <- openFile dot_snips' ReadWriteMode     
        mapM (hPutStr snips_handle' . save_format) 
          (filter (\sn -> title sn /= title s) snips)
        mapM hClose [snips_handle, snips_handle']
        removeFile dot_snips
        renameFile dot_snips' dot_snips
        
copy_snip :: Snip -> IO ()
copy_snip s = do 
  snips <- liftM parse_snips $ readFile dot_snips
  system $ "echo " ++ (contents $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips) ++ " | xclip -selection c" 
  putStrLn "Copied!"
        
clear_snips :: Snip -> IO ()
clear_snips _ = do
  orig_b <- hGetBuffering stdin
  hSetBuffering stdin LineBuffering
  putStrLn "Are you sure you want to clear your Snips library? (y/n):"
  inp <- getChar
  hSetBuffering stdin orig_b
  when (inp == 'y') $ openFile dot_snips WriteMode >>= hClose