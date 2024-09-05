module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--Task 1.1
type Vname = (String)
--Task 1.2
type Val = (Int)
--Task 1.3
type State = (Map Vname Val)

--Task 1.4
data Instr =  ADD
        | LOADI Val
        | LOAD Vname | STORE Vname
        | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--Task 1.5
type Stack = ([Val])

--Task 1.6
type Config = (Int, State, Stack)

--Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (pc, state, stack) = (pc + 1, state, stack ++ [x])
--Add 1 to the inputted program counter, and add x to the top of the inputted stack
iexec (LOAD v) (pc, state, stack) = (pc + 1, state, stack ++ [state ! v])
--Add 1 to the inputted program counter, and add the value found at v within the inputted state to the top of the inputted stack
iexec (STORE v) (pc, state, stack) = (pc + 1, insert v (last(stack)) state, init(stack))
--Add 1 to the inputted program counter, insert the value at the top of the stack into the state with Vname v, and remove the top value from the inputted stack
iexec (ADD) (pc, state, stack) = (pc + 1, state, (init(init(stack))) ++ [last(init(stack)) + last(stack)])
--Add 1 to the inputted program counter, and add the top two values of the stack added together to the top of an inputted stack with the top 2 values removed
iexec (JMP i) (pc, state, stack) = (pc + i + 1, state, stack)
--Add 1 + i to the inputted program counter
iexec (JMPLESS i) (pc, state, stack) | (last(stack) < last(init(stack))) = (pc + i + 1, state, init(init(stack)))
                                     | otherwise = (pc + 1, state, init(init(stack)))
                                     -- Add 1 to the inputted program counter, and if the top of the inputted stack < value beneath, also add i
                                     -- Also remove the top two values of the stack at the end
iexec (JMPGE i) (pc, state, stack) | (last(stack) >= last(init(stack))) = (pc + i + 1, state, init(init(stack)))
                                   | otherwise = (pc + 1, state, init(init(stack)))
                                   -- The same as JMPLESS only instead the i is added to PC if the top of the inputted stack >= value beneath

--Task 1.8
exec :: [Instr] -> Config -> Config
exec [] conf = conf
--If there is an empty list of instructions, return the inputted config
exec list conf = exec (tail list) (iexec (head list) conf)
--Otherwise, use the config from executing the head of the list with the remainder of the list