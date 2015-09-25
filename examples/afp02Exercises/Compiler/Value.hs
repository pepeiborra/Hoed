{-# LANGUAGE DeriveGeneric #-}
module Value(Value(..), Op1(..), Op2(..), uno, duo) where
import Debug.Hoed.Pure

data Value
  = Num Int
  | Log Bool
  | Wrong
  deriving (Eq, Show,Generic)

instance Observable Value

data Op1
  = Not
  | Minus
  deriving (Eq, Show, Generic)

instance Observable Op1

data Op2
  = And
  | Or
  | Mul
  | Add
  | Sub
  | Div
  | Mod
  | Less
  | LessEq 
  | Eq
  deriving (Eq, Show, Generic)

instance Observable Op2

uno :: Op1 -> Value -> Value
uno Not   (Log b) = Log (not b)
uno Minus (Num n) = Num (negate n)
uno _     _       = Wrong

duo :: Op2 -> Value -> Value -> Value
duo And     (Log a) (Log b)          = Log (a && b)
duo Or      (Log a) (Log b)          = Log (a || b)
duo Eq      (Log a) (Log b)          = Log (a == b)
duo Mul     (Num m) (Num n)          = Num (m * n)
duo Add     (Num m) (Num n)          = Num (m + n)
duo Sub     (Num m) (Num n)          = Num (m - n)
duo Div     (Num m) (Num n) | n /= 0 = Num (m `div` n)
duo Mod     (Num m) (Num n) | n /= 0 = Num (m `mod` n)
duo Less    (Num m) (Num n)          = Log (m < n)
duo LessEq  (Num m) (Num n)          = Log (m <= n)
duo Eq      (Num m) (Num n)          = Log (m == n)
duo _       _       _                = Wrong
