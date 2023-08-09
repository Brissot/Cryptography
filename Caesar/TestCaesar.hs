import System.IO
import System.Environment
import Control.Monad
import Data.Maybe
import Text.Read

import Caesar

main= do
      args <- getArgs

      let filename= head args
      handle <- openFile filename ReadMode
      contents <- hGetContents handle
      
      let tails= tail args
      let shift= read (head tails) :: Integer
      putStr (encodeCaesar contents shift)
