module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--Task 2.1
data AExp = N Int
    | V Vname
    | Plus AExp AExp
    deriving (Eq, Read, Show)

--Task 2.2
aval :: AExp -> State -> Val
aval (N x) (state) = x
--Return x
aval (V v) (state) = (state ! v)
--Return the value with the Vname v from the inputted state
aval (Plus a1 a2) (state) = (aval a1 state) + (aval a2 state)
--Return the value of the first expression added to the second expression

--Task 2.1
data BExp = Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
    deriving (Eq, Read, Show)

--Task 2.3
bval :: BExp -> State -> Bool
bval (Bc True) (state) = True
--Return true
bval (Bc False) (state) = False
--Return false
bval (Not x) (state) | (bval x (state)) == True = False
                     | otherwise = True
                     -- If the boolean expression evaluates to true, return false. If it evaluates to false, return true
bval (And b1 b2) (state) = (b1 == b2)
--Return if the expression b1 is the same as b2
bval (Less exp1 exp2) (state) = (aval exp1 state) < (aval exp2 state)
--Return if the first arithmatic expression is less than the second arithmatic expression

--Task 2.1
data Com = Assign Vname AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    | SKIP
    deriving (Eq, Read, Show)

--Task 2.4
eval :: Com -> State -> State
eval (Assign v x) (state) = (insert v (aval x state) state)
--Return the inputted state where the result of evaluating the arithmatic expression is stored with the vname v
eval (Seq c1 c2) (state) = (eval c2 (eval c1 state))
--Return the evaluation of c2 with the result of evaluating c1 with the inputted state
eval (If b c1 c2) (state) | bval b (state) = eval c1 (state) 
                          | otherwise = eval c2 (state)
                          --If b evaluates to true, evaluate c1, otherwise, evaluate c2
eval (While b c) (state) | (bval b (state)) = eval (While b c) (eval c state)
                         | otherwise = state
                          --If b evaluates to true, evaluate the expression again using the state produced from evaluating c with the inputted state. Otherwise, return the state
eval (SKIP) (state) = state
--Return the state unchanged