module Lib.Lang1 ( VarRef(..), Statement(..), Exp(..),   demo, sample) where
-- Data types à la carte: exercises.

-- Begin with the toy language expressed via direct recursion.

data VarRef = Var String
            | FieldRef VarRef String

data Statement = Assign VarRef Exp
               | Seq Statement Statement

data Exp = Mul Exp Exp
         | IntExp Int
         | VarExp VarRef


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

sample :: Statement
sample = Seq (Assign (Var "a") (IntExp 1)) (Assign (Var "b") (Mul  (IntExp 2) (VarExp (Var "a"))))

demo :: IO ()
demo = do
    print sample

-- Implement the same language using DTALC techniques. Implement isomorphism between both types.
