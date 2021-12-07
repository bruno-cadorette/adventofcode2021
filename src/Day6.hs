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
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Day6 where
    
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits
import Data.Type.Ord


type family Count n xs where
  Count n (n : xs) = 1 + Count n xs
  Count n (x : xs) = Count n xs
  Count n '[]  = 0

type family Parse n input where
  Parse 9 xs = '[]
  Parse n xs = Count n xs : Parse (n + 1) xs

type Input =  '[1,1,1,1,2,1,1,4,1,4,3,1,1,1,1,1,1,1,1,4,1,3,1,1,1,5,1,3,1,4,1,2,1,1,5,1,1,1,1,1,1,1,1,1,1,3,4,1,5,1,1,1,1,1,1,1,1,1,3,1,4,1,1,1,1,3,5,1,1,2,1,1,1,1,4,4,1,1,1,4,1,1,4,2,4,4,5,1,1,1,1,2,3,1,1,4,1,5,1,1,1,3,1,1,1,1,5,5,1,2,2,2,2,1,1,2,1,1,1,1,1,3,1,1,1,2,3,1,5,1,1,1,2,2,1,1,1,1,1,3,2,1,1,1,4,3,1,1,4,1,5,4,1,4,1,1,1,1,1,1,1,1,1,1,2,2,4,5,1,1,1,1,5,4,1,3,1,1,1,1,4,3,3,3,1,2,3,1,1,1,1,1,1,1,1,2,1,1,1,5,1,3,1,4,3,1,3,1,5,1,1,1,1,3,1,5,1,2,4,1,1,4,1,4,4,2,1,2,1,3,3,1,4,4,1,1,3,4,1,1,1,2,5,2,5,1,1,1,4,1,1,1,1,1,1,3,1,5,1,2,1,1,1,1,1,4,4,1,1,1,5,1,1,5,1,2,1,5,1,1,1,1,1,1,1,1,1,1,1,1,3,2,4,1,1,2,1,1,3,2]

type family Step xs where
  Step '[v0, v1, v2, v3, v4, v5, v6, v7, v8] = '[v1, v2, v3, v4, v5, v6, v7 + v0, v8, v0]
  Step _ = TypeError (Text "Must have 9 elements")

type family AfterNDays n xs where
  AfterNDays 0 xs = xs
  AfterNDays n xs = AfterNDays (n - 1) (Step xs)

type family Sum xs where
  Sum '[] = 0
  Sum (x:xs) = x + Sum xs

type Solution days input = Sum (AfterNDays days (Parse 0 input))