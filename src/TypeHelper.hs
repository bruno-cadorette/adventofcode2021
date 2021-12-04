{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-} 
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeHelper (module TypeHelper) where

import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits
import Data.Type.Ord

type MaybeTupleToList :: Maybe (Char, Symbol) -> [Char]
type family MaybeTupleToList mTuple where
  MaybeTupleToList Nothing = '[]
  MaybeTupleToList (Just '(x, xs)) = x : SymbolToList xs

type SymbolToList :: Symbol -> [Char]
type family SymbolToList symbol where
  SymbolToList str = MaybeTupleToList (UnconsSymbol str) 