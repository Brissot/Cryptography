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
      message <- hGetContents handle
      let args2= tail args

      let filename2= head args2
      handle2 <- openFile filename2 ReadMode
      cypher <- hGetContents handle2
      
      let encoded= encodeVigenere message cypher
      putStr encoded
      putStr (decodeVigenere encoded cypher)
