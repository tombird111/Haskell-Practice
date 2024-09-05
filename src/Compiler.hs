module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
--Return the command that would load x into the stack
acomp (V v) = [LOAD v]
--Return the command that would load the value stored with Vname v into the stack
acomp (Plus a1 a2) = (acomp a1) ++ (acomp a2) ++ [ADD]
--Return the instructions for the first expression, followed by the second expressions instructions, then append the [ADD] instruction

--Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b) (jumpbool) (jumplength) | (jumplength == 0) = []
                                     | (jumpbool == b) = [JMP jumplength]
                                     | otherwise = []
                                     -- Return nothing if either the jump would not jump at all, or if the inputted bool != the inputted bool for jumping
                                     -- If the bool for jumping == inputted boolean, then return a jmp that would move the inputted length
bcomp (Not b) (jumpbool) (jumplength) | (jumpbool == True) = (bcomp (b) (False) (jumplength))
                                      | otherwise = (bcomp (b) (True) (jumplength))
                                      -- Return the instructions for the inverse of a given expression
bcomp (And b1 b2) (jumpbool) (jumplength) | (jumpbool == True) = (bcomp (b1) (not jumpbool) (length (bcomp (b2) (jumpbool) (jumplength)))) ++ (bcomp (b2) (jumpbool) (jumplength))
                                          | otherwise = (bcomp (b1) (jumpbool) (jumplength + length (bcomp (b2) (False) (jumplength)))) ++ (bcomp (b2) (jumpbool) (jumplength))
                                          --If both values must be true, add the required instructions for b2, and then add the instructions that would jump over b2's instructions if b1 is false
                                          --Otherwise, create the instructions for b1 to jump the distance, alongwith any instructions potentially created by b2. Then add b2's instructions
bcomp (Less exp1 exp2) (jumpbool) (jumplength) | (jumpbool) = ((acomp (exp1)) ++ (acomp (exp2))) ++ [JMPLESS jumplength]
                                               | otherwise = ((acomp (exp1)) ++ (acomp (exp2))) ++ [JMPGE jumplength]
                                               --If the expression must evaluate to true to jump, add the instructions for expression 1 followed by expression 2, then the instruction JMPLESS with the jump distance
                                               --Otherwise, add a JMPGE instead

--Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign v x) = (acomp x) ++ [STORE v]
--Append the instruction [STORE v] to the end of the instructions that evaluate the arithmatic expression x
ccomp (Seq c1 c2) = (ccomp c1) ++ (ccomp c2)
--Append the instructions from compiling c2 to the end of the instructions for compiling c1
ccomp (If b c1 c2) = (bcomp b (False) (length (ccomp c1) + 1)) ++ (ccomp c1) ++ [JMP (length (ccomp c2))] ++ (ccomp c2)
--Compile b with the jump length that would go over all instructions from compiling c1 and a single command
--Follow this with the instructions from compiling c1
--Them add a jump that jumps over the instructions compiled from c2
--Then add the instructions compiled from c2
ccomp (While b c) = (bcomp b (False) (length (ccomp c) + 1)) ++ (ccomp c) ++ [JMP (-1 - length ((ccomp c) ++ (bcomp b (False) (length (ccomp c) + 1))))]
--Add a jump that, in the event of b evaluating to false, jumps over the instructions from compiling c
--Add the instructions from compiling c
--Add a jump that jumps to the initial check