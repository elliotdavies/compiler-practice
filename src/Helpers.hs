module Helpers where

import Data.List (intersperse)

join :: String -> [String] -> String
join s = foldr (++) "" . intersperse s
