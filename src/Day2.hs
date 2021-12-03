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

module Day2 where
    
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits
import Data.Type.Ord

data Direction = Up | Down | Forward

data Command = CommandI Direction Natural

type MaybeTupleToList :: Maybe (Char, Symbol) -> [Char]
type family MaybeTupleToList mTuple where
  MaybeTupleToList Nothing = '[]
  MaybeTupleToList (Just '(x, xs)) = x : SymbolToList xs

type SymbolToList :: Symbol -> [Char]
type family SymbolToList symbol where
  SymbolToList str = MaybeTupleToList (UnconsSymbol str) 

type CharToNatValue :: Char -> Natural
type family CharToNatValue chr where
  CharToNatValue chr = CharToNat chr - CharToNat '0'

type ParseCommand :: [Char] -> Command
type family ParseCommand str where
  ParseCommand ['f', 'o', 'r', 'w', 'a', 'r', 'd', ' ', n] = CommandI Forward (CharToNatValue n) 
  ParseCommand ['u', 'p', ' ', n] = CommandI Up (CharToNatValue n)
  ParseCommand ['d', 'o', 'w', 'n', ' ', n] = CommandI Down (CharToNatValue n)

type ParseInput :: [Symbol] -> [Command]
type family ParseInput lst where
  ParseInput (x:xs) = ParseCommand(SymbolToList x) : ParseInput xs
  ParseInput '[] = '[]

type Solve1 :: (Natural, Natural) -> [Command] -> Natural
type family Solve1 cmds pos where
  Solve1 '(horizontal, depth) '[] = horizontal * depth
  Solve1 '(horizontal, depth) (CommandI Forward n : xs) = Solve1 '(horizontal + n, depth) xs
  Solve1 '(horizontal, depth) (CommandI Down n : xs) = Solve1 '(horizontal, depth + n) xs
  Solve1 '(horizontal, depth) (CommandI Up n : xs) = Solve1 '(horizontal, depth - n) xs

type Solve2 :: (Natural, Natural, Natural) -> [Command] -> Natural
type family Solve2 cmds pos where
  Solve2 '(horizontal, depth, aim) '[] = horizontal * depth
  Solve2 '(horizontal, depth, aim) (CommandI Forward n : xs) = Solve2 '(horizontal + n, depth + (aim * n), aim) xs
  Solve2 '(horizontal, depth, aim) (CommandI Down n : xs) = Solve2 '(horizontal, depth, aim + n) xs
  Solve2 '(horizontal, depth, aim) (CommandI Up n : xs) = Solve2 '(horizontal, depth, aim - n) xs

type Solution1 = Solve1 '(0, 0) Input

type Solution2 = Solve2 '(0, 0, 0) Input

type Input = ParseInput '["forward 7","forward 9","forward 3","down 5","down 9","forward 6","down 2","forward 2","forward 8","forward 3","forward 5","forward 5","forward 8","down 6","forward 8","forward 2","up 8","down 8","forward 6","down 4","down 5","forward 2","down 6","forward 7","down 9","forward 9","down 2","down 7","up 6","up 3","up 7","down 9","forward 1","forward 1","down 4","down 9","forward 4","up 4","forward 8","forward 9","down 7","down 4","up 6","down 8","down 2","forward 8","forward 6","down 3","forward 2","forward 6","down 3","forward 1","forward 8","down 8","down 9","forward 5","forward 4","forward 8","down 7","forward 4","forward 3","forward 6","down 3","forward 6","forward 6","down 9","down 9","down 9","down 2","down 7","down 4","forward 3","up 7","up 3","down 1","forward 4","up 9","forward 4","forward 2","down 2","forward 9","up 4","forward 5","down 8","up 7","down 5","down 1","up 7","up 4","forward 5","up 8","up 3","down 2","down 1","down 2","forward 3","up 1","forward 1","forward 1","down 1","down 6","down 6","up 4","down 4","down 4","forward 6","down 6","forward 7","forward 5","up 7","down 9","down 6","forward 5","forward 6","forward 2","down 4","forward 5","forward 8","down 8","down 6","forward 2","forward 8","down 3","forward 6","down 1","forward 5","down 8","up 1","forward 6","down 7","forward 4","down 8","down 8","forward 8","down 6","down 3","forward 2","forward 8","forward 9","forward 4","forward 3","down 4","forward 3","down 9","down 1","forward 2","forward 3","forward 7","down 1","forward 6","forward 8","forward 6","forward 2","down 8","up 9","forward 6","forward 8","down 7","down 5","up 4","forward 9","up 7","up 3","forward 3","down 6","forward 4","forward 2","down 3","forward 9","forward 5","up 7","down 9","up 4","down 3","forward 8","up 1","forward 2","forward 8","forward 8","forward 5","down 7","up 6","down 9","down 4","forward 2","down 5","down 2","down 2","forward 6","down 2","forward 9","forward 1","up 1","forward 4","down 1","forward 3","down 3","forward 4","up 5","up 3","forward 6","forward 8","forward 2","forward 6","up 5","down 9","down 8","forward 3","down 5","forward 8","forward 1","down 9","up 3","down 2","down 9","up 8","down 2","up 7","up 2","up 3","down 9","down 1","down 7","down 1","forward 1","down 9","down 6","forward 3","up 7","up 8","down 5","down 6","up 2","forward 8","down 4","up 1","forward 4","up 4","forward 2","down 4","forward 4","down 9","up 4","forward 8","up 7","forward 1","down 3","up 7","forward 5","down 5","forward 2","forward 7","forward 3","down 8","forward 4","forward 9","up 2","down 4","down 5","forward 4","down 4","up 6","down 8","up 1","down 1","up 6","up 6","down 7","down 7","forward 2","forward 4","forward 8","down 8","down 4","down 4","down 7","forward 4","down 3","forward 5","forward 5","forward 7","down 7","forward 1","down 8","up 4","up 9","up 3","up 6","forward 5","forward 5","forward 4","forward 9","down 9","forward 4","forward 1","up 8","up 2","down 9","up 4","forward 2","up 8","forward 6","forward 2","up 9","down 3","forward 3","up 7","down 7","forward 4","forward 7","forward 3","down 4","down 5","forward 7","up 3","up 1","down 4","forward 6","down 1","forward 1","down 4","down 3","forward 9","forward 4","down 9","down 3","forward 2","forward 5","forward 6","down 3","forward 5","down 9","forward 2","forward 9","down 7","down 4","down 3","down 1","up 2","forward 6","forward 4","down 9","down 2","forward 2","forward 9","down 3","forward 8","down 8","forward 5","down 4","forward 4","up 6","up 3","down 3","down 9","forward 5","forward 8","down 2","forward 9","forward 5","up 9","forward 2","forward 3","forward 4","up 8","up 1","up 6","down 5","down 8","down 4","forward 6","up 2","forward 1","forward 7","up 8","forward 5","up 9","forward 7","down 6","up 5","up 7","up 1","down 3","up 6","forward 1","up 1","forward 2","forward 4","forward 5","up 3","up 8","up 1","up 6","up 3","down 5","down 4","up 8","down 9","up 7","down 6","down 9","forward 5","forward 3","down 9","down 3","down 6","up 3","up 8","down 4","down 1","up 9","up 9","forward 8","down 7","forward 1","forward 4","down 8","forward 2","down 4","forward 7","forward 3","forward 5","forward 1","up 2","down 9","down 5","up 6","down 3","forward 1","up 9","forward 6","forward 1","forward 4","up 7","forward 6","down 1","forward 9","forward 1","forward 3","down 9","down 8","down 5","forward 4","down 7","up 1","forward 8","up 4","forward 6","down 2","forward 4","forward 7","down 8","forward 6","down 7","forward 7","up 7","forward 4","down 8","down 8","forward 8","forward 6","down 9","down 8","down 6","down 2","down 4","forward 7","forward 3","down 8","down 5","forward 2","down 9","down 7","up 1","up 5","forward 6","up 8","up 7","up 4","down 6","down 6","down 8","down 9","down 2","forward 6","forward 6","forward 2","up 9","forward 6","forward 9","forward 8","down 5","down 3","forward 1","forward 8","forward 1","forward 3","down 4","forward 5","forward 1","forward 6","down 8","down 9","forward 3","forward 2","forward 1","forward 3","up 7","down 7","down 2","forward 3","down 5","down 2","down 7","down 9","down 5","down 7","down 9","up 7","forward 7","forward 9","forward 8","forward 5","down 1","up 6","up 6","forward 5","up 6","down 8","up 6","forward 2","down 9","down 5","up 8","up 7","down 8","down 7","up 3","down 5","forward 6","forward 2","down 6","forward 6","forward 1","forward 5","forward 3","down 4","forward 3","down 1","up 7","forward 3","forward 9","forward 3","forward 4","down 9","forward 6","down 1","up 6","forward 2","forward 1","down 2","down 1","down 9","forward 1","up 8","down 1","up 3","forward 3","forward 1","up 6","down 1","down 7","down 2","forward 5","down 4","forward 4","forward 9","down 7","forward 6","down 4","forward 8","down 5","forward 6","down 6","down 6","down 9","forward 3","forward 2","forward 7","forward 6","forward 8","up 6","forward 7","down 2","up 4","forward 6","forward 3","forward 9","down 1","forward 9","down 1","forward 6","down 9","forward 7","forward 9","forward 6","up 3","down 3","forward 3","up 1","down 8","forward 7","down 4","forward 7","forward 7","down 1","forward 5","down 6","forward 6","down 8","down 2","down 7","forward 9","forward 7","forward 2","down 5","forward 7","forward 8","forward 5","forward 5","up 1","down 1","up 4","forward 5","forward 8","down 4","up 8","forward 8","up 2","down 1","down 9","up 9","down 9","forward 3","forward 1","down 7","down 2","forward 5","up 7","forward 9","forward 1","down 4","down 8","down 2","up 1","up 6","forward 9","down 3","down 2","forward 5","forward 4","down 5","down 4","up 4","forward 4","down 3","up 3","down 7","down 7","forward 1","forward 4","forward 7","forward 5","down 4","down 7","forward 1","forward 9","down 4","forward 8","up 4","down 9","down 9","up 6","up 3","forward 2","forward 3","up 7","forward 7","down 4","forward 5","forward 5","up 2","down 5","down 9","forward 9","forward 7","forward 1","up 5","up 5","forward 8","forward 3","forward 2","down 4","down 6","down 2","forward 5","down 3","down 9","forward 8","forward 7","forward 7","down 1","up 3","down 8","down 9","forward 6","up 6","down 6","forward 2","forward 3","forward 7","up 8","down 8","down 7","forward 2","down 2","up 7","up 9","forward 1","forward 1","forward 1","forward 1","forward 1","up 8","down 3","up 8","down 5","down 3","up 4","forward 4","down 3","down 4","down 3","up 3","down 3","up 2","up 6","down 9","down 6","up 8","up 7","down 1","down 7","down 3","forward 3","forward 5","down 4","down 7","forward 1","forward 8","up 9","up 2","forward 3","up 1","forward 7","down 7","down 5","forward 9","up 9","forward 3","down 2","up 4","down 2","down 1","down 9","down 9","forward 3","forward 4","down 2","down 6","up 8","down 5","forward 7","forward 4","up 3","forward 2","down 4","down 8","forward 4","forward 6","forward 8","down 6","down 8","up 2","forward 5","up 7","down 9","down 6","forward 7","up 3","down 9","forward 2","down 6","up 6","down 6","down 3","down 2","down 8","down 4","forward 8","up 7","forward 9","forward 4","down 3","forward 3","down 9","down 2","forward 2","forward 1","down 4","down 3","down 8","up 6","down 4","forward 3","down 7","forward 8","down 7","forward 6","forward 2","forward 7","forward 6","forward 4","up 4","forward 2","down 4","down 2","forward 3","down 2","up 9","down 6","forward 5","up 6","forward 1","up 1","down 3","up 4","forward 1","down 6","forward 9","up 2","forward 4","up 9","up 5","down 5","forward 3","down 9","forward 5","down 3","forward 7","forward 5","forward 9","up 5","down 4","down 2","forward 9","down 3","down 8","down 9","forward 2","down 8","up 6","down 4","down 2","up 9","forward 8","forward 8","down 8","forward 4","down 7","forward 2","up 7","forward 7","down 4","forward 4","down 3","forward 9","down 9","forward 6","down 5","down 9","up 5","forward 7","forward 2","down 3","down 7","down 2","forward 3","down 4","up 3","down 1","forward 9","down 4","down 8","up 9","forward 7","down 8","forward 9","down 2","up 2","down 1","down 1","forward 6","forward 2","forward 3","down 5","down 1","down 1","up 4","forward 8","down 3","down 1","forward 9","forward 7","forward 2","up 8","up 6","down 7","down 6","forward 3","down 2","down 9","up 7","forward 5","up 9","down 9","down 4","down 8","down 5","down 8","down 8","forward 6","forward 1","forward 4","forward 7","down 7","down 6","forward 4","forward 7","forward 6","down 7","forward 4","forward 9","up 3","forward 9","forward 5","forward 1","up 2","down 1","down 5","forward 9","up 4","forward 6","up 3","up 6","forward 8","down 6","forward 5","down 3","forward 2","forward 7","down 4","up 8","forward 6","up 7","up 9","forward 3","down 3","down 7","down 7","down 1","down 6","down 9","up 1","forward 6","forward 6","down 3","forward 7","down 8","forward 1","down 7","down 4","down 3","down 4","down 4","forward 7","down 3","forward 6","up 9","forward 3"]

type ExampleInput = ParseInput '["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

proofPart1 :: Solve1 '(0,0) ExampleInput :~: 150
proofPart1 = Refl

proofPart2 :: Solve2 '(0,0,0) ExampleInput :~: 900
proofPart2 = Refl




data TypeInt = 
    Positive Natural
  | Negative Natural

type family Add a b where
  Add (Positive a) (Positive b) = Positive (a + b)
  Add (Negative a) (Positive b) = If (a >? b) (Negative (a - b)) (Positive (b - a))
  Add (Positive a) (Negative b) = Add (Negative b) (Positive a)
  Add (Negative a) (Negative b) = Negative (a + b)
