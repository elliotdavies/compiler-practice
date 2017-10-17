module Helpers where

import Data.List (intersperse)

-- Join strings on the given string
join :: String -> [String] -> String
join s = foldr (++) "" . intersperse s
