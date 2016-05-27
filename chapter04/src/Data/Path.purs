module Data.Path
  ( Path()
  , root
  , ls
  , filename
  , isDirectory
  , size
  , allFiles
  , onlyFiles
  ) where

import Prelude

import Data.Maybe
import Data.Array
import Data.Foldable

data Path
  = Directory String (Array Path)
  | File String Int

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
        [ File "/bin/cp" 24800
        , File "/bin/ls" 34700
        , File "/bin/mv" 20200
        ]
    , Directory "/etc/"
        [ File "/etc/hosts" 300
        ]
    , Directory "/home/"
        [ Directory "/home/user/"
            [ File "/home/user/todo.txt" 1020
            , Directory "/home/user/code/"
                [ Directory "/home/user/code/js/"
                    [ File "/home/user/code/js/test.js" 40000
                    ]
                , Directory "/home/user/code/haskell/"
                    [ File "/home/user/code/haskell/test.hs" 5000
                    ]
                ]
            ]
        ]
    ]

filename :: Path -> String
filename (File name _) = name
filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false

ls :: Path -> Array Path
ls (Directory _ xs) = xs
ls _ = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes
size _ = Nothing

allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child


-- 4.14 (Easy) Write a function onlyFiles which returns all files
-- (not directories) in all subdirectories of a directory.
onlyFiles :: Path -> Array Path
onlyFiles = filter (not <<< isDirectory) <<< allFiles


-- 4.15 (Medium) Write a fold to determine the largest and smallest files in
-- the filesystem.
{-- largerFile --} 
{-- largestFile :: Path -> Path --}
{-- largestFile path = foldl (\acc item -> ) <<< onlyFiles path --}
