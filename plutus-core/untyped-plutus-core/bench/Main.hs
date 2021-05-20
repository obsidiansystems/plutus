{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Function
import System.Random
import Unsafe.Coerce

import Data.BRAL as B
import Data.IntMap.Strict  as I

main :: IO ()
main = defaultMain
    -- NOTE: there is a faster/better way to create a map using fromAscList
    -- but we want to bench cons-ing because that is what we are using in our machine.
    [ bgroup "create/1000" [ bench "bral" $ whnf (extendB B.nil) 1000
                           , bench "imap" $ whnf (extendI I.empty) 1000
                           ]
    , bgroup "create/10000" [ bench "bral" $ whnf (extendB B.nil) 10000
                           , bench "imap" $ whnf (extendI I.empty) 10000
                            ]


    , bgroup "query/front/1000" [ bench "bral" $ whnf (queryFrontB 1000) b1000
                                , bench "imap" $ whnf (queryFrontI 1000) i1000
                                ]
    , bgroup "query/front/10000" [ bench "bral" $ whnf (queryFrontB 10000) b10000
                                , bench "imap" $ whnf (queryFrontI 10000) i10000
                                ]

    , bgroup "query/back/1000" [ bench "bral" $ whnf (queryBackB 1000) b1000
                               , bench "imap" $ whnf (queryBackI 1000) i1000
                               ]
    , bgroup "query/back/10000" [ bench "bral" $ whnf (queryBackB 10000) b10000
                               , bench "imap" $ whnf (queryBackI 10000) i10000
                               ]


    , bgroup "query/rand/1000" [ bench "bral" $ whnf (uncurry queryRandB) (rand1000, b1000)
                               , bench "imap" $ whnf (uncurry queryRandI) (rand1000, i1000)
                               ]
    , bgroup "query/rand/10000" [ bench "bral" $ whnf (uncurry queryRandB) (rand10000, b10000)
                               , bench "imap" $ whnf (uncurry queryRandI) (rand10000, i10000)
                               ]

    , bgroup "create1000/front1000/cons1000/back1000/cons1000/rand1000"
            [ bench "bral" $ whnf (uncurry $ mixB 1000 1000 1000 1000) (rand1000, b1000)
            , bench "imap" $ whnf (uncurry $ mixI 1000 1000 1000 1000) (rand1000, i1000)
            ]
    , bgroup "create10000/front1000/cons1000/back1000/cons1000/rand10000"
            [ bench "bral" $ whnf (uncurry $ mixB 1000 1000 1000 1000) (rand10000, b10000)
            , bench "imap" $ whnf (uncurry $ mixI 1000 1000 1000 1000) (rand10000, i10000)
            ]


    ]
  where
        b1000 = extendB B.nil 1000
        i1000 = extendI I.empty 1000
        b10000 = extendB B.nil 10000
        i10000 = extendI I.empty 10000
        rand1000 = take 1000 $ randomRs (1,1000) g
        rand10000 = take 10000 $ randomRs (1,10000) g
        g = mkStdGen 59950

extendB :: BList () -> Word -> B.BList ()
extendB initB = fix $
    \ rec n -> if n == 0
              then initB
              else B.cons () $ rec (n-1)


extendI :: I.IntMap () ->  Int -> I.IntMap ()
extendI initI  = fix $
    \ rec n -> if n == 0
              then initI
              else I.insert n () $ rec (n-1)


queryFrontB :: Word -> B.BList () -> ()
queryFrontB 1 d = index d 1
queryFrontB !i d =
    index d i `seq` queryFrontB (i-1) d

queryBackB :: Word -> B.BList () -> ()
queryBackB size = go 1
 where
   go !i d | i == size = index d i
           | otherwise = index d i `seq` go (i+1) d

queryFrontI :: Int -> I.IntMap () -> ()
queryFrontI 1 d = d I.! 1
queryFrontI !i d = d I.! i `seq` queryFrontI (i-1) d

queryBackI :: Int -> I.IntMap () -> ()
queryBackI size = go 1
 where
   go !i d | i == size = d I.! i
           | otherwise = d I.! i `seq` go (i+1) d


queryRandB :: [Word] -> B.BList () -> ()
queryRandB [] _ = ()
queryRandB (i:is) d = index d i `seq` queryRandB is d

queryRandI :: [Word] -> I.IntMap () -> ()
queryRandI [] _ = ()
queryRandI (i:is) d = d I.! unsafeCoerce i `seq` queryRandI is d

mixB :: Word -> Word -> Word -> Word -> [Word] -> BList () -> ()
mixB front cons1 back cons2 rand d =
    queryFrontB front d
    `seq`
    let d1 = extendB d cons1
    in queryBackB back d1
    `seq`
    let d2 = extendB d1 cons2
    in queryRandB rand d2



mixI :: Int -> Int -> Int -> Int -> [Word] -> I.IntMap () -> ()
mixI front cons1 back cons2 rand d =
    queryFrontI front d
    `seq`
    let d1 = extendI d cons1
    in queryBackI back d1
    `seq`
    let d2 = extendI d1 cons2
    in queryRandI rand d2



