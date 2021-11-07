{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.List (foldl', permutations)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import System.Random (mkStdGen, randomRs)
import Web.Routing.Combinators
import Web.Routing.SafeRouting

buildPath :: [T.Text] -> PathInternal '[]
buildPath = toInternalPath . static . T.unpack . T.intercalate "/"

buildPathMap :: [([T.Text], a)] -> PathMap a
buildPathMap =
  foldl' (\t (route, val) -> insertPathMap' (buildPath route) (const val) t) emptyPathMap

lookupPathMapM :: [[T.Text]] -> PathMap Int -> Int
lookupPathMapM rs m =
  foldl' (\z route -> fromMaybe z (listToMaybe $ match m route)) 0 rs

benchmarks :: [Benchmark]
benchmarks =
  [ env setupSafeMap $ \ ~(safeMap, routes') ->
      bgroup
        "SafeRouting"
        [ bench "static-lookup" $ whnf (lookupPathMapM routes') safeMap
        ]
  ]
  where
    strlen = 10
    seglen = 5
    num = 10
    routes = rndRoutes strlen seglen num
    routesList = zip routes [1 ..]
    setupSafeMap = return (buildPathMap routesList, routes)

main :: IO ()
main = defaultMain benchmarks

chunks :: Int -> [a] -> [[a]]
chunks n xs =
  let (ys, xs') = splitAt n xs
   in ys : chunks n xs'

-- | Generate a number of paths consisting of a fixed number of fixed length
-- strings ("path segments") where the content of the segments are letters in
-- random order. Contains all permutations with the path.
rndRoutes ::
  -- | Length of each string
  Int ->
  -- | Number of segments
  Int ->
  -- | Number of routes
  Int ->
  [[T.Text]]
rndRoutes strlen seglen num =
  take num $
    concatMap permutations $
      chunks seglen $
        map T.pack $
          chunks strlen $ randomRs ('a', 'z') $ mkStdGen 1234
