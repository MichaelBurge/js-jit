module Main where

import JS.Special
import JS.Assembler

main :: IO ()
main = do
  let bs = assemble [IRet]
  fun <- load bs
  exec fun
  putStrLn $ show a_log
