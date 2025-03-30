module Lib.Lang1 where

-- ( VarRef(..), Statement(..), Exp(..),   demo, sample, GetInts(..))

-- Data types à la carte: exercises.

-- Begin with the toy language expressed via direct recursion.

data VarRef
  = Var String
  | FieldRef VarRef String

data Statement
  = Assign VarRef Exp
  | Seq Statement Statement

data Exp
  = Mul Exp Exp
  | IntExp Int
  | VarExp VarRef

-- Convert the language to String. This is done via the Show typeclass.

instance Show VarRef where
  show (Var s) = s
  show (FieldRef r s) = show r ++ "." ++ s

instance Show Statement where
  show (Assign v e) = show v ++ " := " ++ show e ++ ";"
  show (Seq s1 s2) = show s1 ++ "\n" ++ show s2

instance Show Exp where
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (IntExp i) = show i
  show (VarExp v) = show v

-- Extract all literal integers from the language.

class GetInts a where
  getInts :: a -> [Int]

instance GetInts VarRef where
  getInts _ = []

instance GetInts Exp where
  getInts (IntExp i) = [i]
  getInts (Mul e1 e2) = getInts e1 ++ getInts e2
  getInts _ = []

instance GetInts Statement where
  getInts (Assign _ e) = getInts e
  getInts (Seq s1 s2) = getInts s1 ++ getInts s2

sample :: Statement
sample = Seq (Assign (Var "a") (IntExp 1)) (Assign (Var "b") (Mul (IntExp 2) (VarExp (Var "a"))))

demo :: IO ()
demo = do
  print sample

-- Implement the same language using DTALC techniques. Implement isomorphism between both types.

-- Pattern functors need to take 3 arguments since we are defining 3 mutually recursive types at once:

data VarRefP r = VarP String | FieldRefP r String

data StatementP r varRefP expP = AssignP varRefP expP | SeqP r r

data ExpP r varRefP = MulP r r | IntExpP Int | VarExpP varRefP
