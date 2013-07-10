import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

data Snip = Snip { title :: String
                 , language :: String
                 , contents :: String
                 } deriving (Eq)
            
instance Show Snip where
  show s = "Title: " ++ title s ++ "\nLang: " ++ language s ++ "\nCont: " ++ contents s

main :: IO ()
main = do
  args <- getArgs
  apply args

apply args
  | head args `elem` ["print","search","remove"] = parse_command (head args) $ 
                                          Snip (args !! 1) "" ""
  | head args == "add"  = let s = tail args in
  add_snip $ Snip (head s) (s !! 1) (s !! 2) 
  | head args == "count" = parse_command (head args) $ Snip "" "" "" 
  | otherwise = putStrLn "Error: Command not regognized."

                                  

parse_command :: String -> (Snip -> IO ())
parse_command str
  | str == "add"    = add_snip
  | str == "print"  = print_snip
  | str == "search" = search_snip
  | str == "count"  = count_snip
  | str == "remove" = remove_snip

parse_snips :: String -> [Snip]
parse_snips str = map mk_snip (lines str)
  where
    mk_snip str = let fields = splitOn "\\" str in
      Snip { title    = fromMaybe "" (stripPrefix "Title: " (head fields))
           , language = fromMaybe "" (stripPrefix "Lang: " (fields !! 1)) 
           , contents = fromMaybe "" (stripPrefix "Cont: " (fields !! 2))
           }

add_snip :: Snip -> IO ()
add_snip s = do
  snips <- liftM parse_snips $ readFile "/home/joejev/.snips" 
  if any (==title s) (map title snips) 
    then putStrLn $ title s ++ " is already in use (use remove to clear the name if you still want to use it)" 
    else 
     appendFile "/home/joejev/.snips" $ intercalate "\\" (splitOn "\n" (show s))

print_snip :: Snip -> IO ()
print_snip s = do 
  snips <- liftM parse_snips $ readFile "/home/joejev/.snips"
  putStrLn $ show $ fromMaybe (Snip "Snip Not Found" "" "") $ 
    find (\sn -> title s == title sn) snips

search_snip :: Snip -> IO ()
search_snip s = do
  snips <- liftM parse_snips $ readFile "/home/joejev/.snips"
  print $ map title $ filter (\sn -> title s `isInfixOf` contents sn) snips 

count_snip :: Snip -> IO ()
count_snip _ = do
  snips <- liftM parse_snips $ readFile "/home/joejev/.snips"
  print $ length (tail snips)

remove_snip :: Snip -> IO ()
remove_snip s =  do 
  snips <- liftM parse_snips $ readFile "/home/joejev/.snips"
  if any (==title s) (map title snips) 
    then writeFile "/home/joejev/.snips" 
         (concatMap show $ filter (\sn -> title sn /= title s) snips)
    else 
     putStrLn $ title s ++ " is not already in use"