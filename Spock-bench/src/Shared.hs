module Shared where

complexDeep :: [(Int, Int, Int)]
complexDeep =
    [(x, y, z) | x <- [0..5], y <- [0..5], z <- [0..5]]
