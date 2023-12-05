main :: IO () 
main = do 
  file <- readFile "input.txt"
  let input = lines $ file
  print $ input
