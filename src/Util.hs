module Util where

import qualified Debug.Trace as T

trace a b = T.trace a b

pair :: [a] -> [(a,a)]
pair [] = []
pair [x] = [(x,x)]
pair (x:xs) = 
  let r =  map (\y -> (x,y)) xs
      k = pair xs
  in r ++ k
   
