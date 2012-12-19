{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JSMap (Map, findWithDefault, sizeM, filterM, toList, emptyM) where

import Language.Fay.Prelude
import Language.Fay.FFI

data Map k a
instance (Foreign k, Foreign a) => Foreign (Map k a)

findWithDefault :: a -> k -> Map k a -> a
findWithDefault = undefined

sizeM :: Map k a -> Int
sizeM = undefined

filterM :: (k -> Bool) -> Map k a -> Map k a
filterM = undefined

toList :: Map k a -> [(k, a)]
toList = undefined

emptyM :: Map k a
emptyM = undefined

