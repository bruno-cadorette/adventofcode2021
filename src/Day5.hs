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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Day5 where
    
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits
import Data.Type.Ord

data Line = Horizontal Nat Nat Nat | Vertical Nat Nat Nat | Diagonal

type family LineOverlap a1 b1 a2 b2 where
  LineOverlap a1 b1 a2 b2 = 
    If (Max a1 a2 <=? Min b1 b2) 
      (Range (Max a1 a2) (Min b1 b2)) 
      '[] 

lineOverlapsTests :: (LineOverlap 1 3 2 4 :~: '[2,3], LineOverlap 1 3 2 4 :~: LineOverlap 2 4 1 3)
lineOverlapsTests = (Refl, Refl)

type family AddX x ys where
  AddX x (y:ys) = '(x,y) : AddX x ys
  AddX x '[] = '[]

type family AddY y xs where
  AddY y (x:xs) = '(x,y) : AddY y xs
  AddY y '[] = '[]

type family FindOverlap l1 l2 where
  FindOverlap (Horizontal from1 to1 y) (Horizontal from2 to2 y) = AddY y (LineOverlap from1 to1 from2 to2)
  FindOverlap (Horizontal x1 x2 hY) (Vertical y1 y2 vX) = If (Between x1 vX x2 && Between y1 hY y2) '[ '(vX, hY) ] '[] 
  FindOverlap (Vertical a b c) (Horizontal d e f) = FindOverlap (Horizontal d e f) (Vertical a b c)
  FindOverlap (Vertical from1 to1 x) (Vertical from2 to2 x) = AddX x (LineOverlap from1 to1 from2 to2)
  FindOverlap _ _ = '[]

type family Range a b where
  Range a a = '[a]
  Range a b = a : Range (a + 1) b

type family Between a b c where
  Between a b c = a <=? b && b <=? c

type family CreateLines x1 y1 x2 y2 where
  CreateLines x y1 x y2 = If (y1 <=? y2) (Vertical y1 y2 x) (Vertical y2 y1 x)
  CreateLines x1 y x2 y = If (x1 <=? x2) (Horizontal x1 x2 y) (Horizontal x2 x1 y)
  CreateLines _ _ _ _ = Diagonal

type ExampleInput = '[
  '[0,9, 5,9],
  '[8,0, 0,8],
  '[9,4, 3,4],
  '[2,2, 2,1],
  '[7,0, 7,4],
  '[6,4, 2,0],
  '[0,9, 2,9],
  '[3,4, 1,4],
  '[0,0, 8,8],
  '[5,5, 8,2]]

type family IntersectWithAll x xs where
  IntersectWithAll l1 (l2:ls) = Append (FindOverlap l1 l2) (IntersectWithAll l1 ls)
  IntersectWithAll l1 '[] = '[]

type family GetAllPoints lines where
  GetAllPoints (x:xs) = Append (IntersectWithAll x xs) (GetAllPoints xs)
  GetAllPoints '[] = '[]

type family Append xs ys where              
  Append '[]    ys = ys                     
  Append (x:xs) ys = x : Append xs ys       

type IsNumberChar c = Between '0' c '9'

type CharToNatValue chr = CharToNat chr - CharToNat '0'

type MaybeTupleToList :: Maybe (Char, Symbol) -> [Char]
type family MaybeTupleToList mTuple where
  MaybeTupleToList Nothing = '[]
  MaybeTupleToList (Just '(x, xs)) = x : SymbolToList xs

type SymbolToList :: Symbol -> [Char]
type family SymbolToList symbol where
  SymbolToList str = MaybeTupleToList (UnconsSymbol str) 

type ParseLine