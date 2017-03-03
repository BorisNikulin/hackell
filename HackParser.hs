import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Maybe

type Symbol = T.Text

data Instruction = AType (Either Int Symbol)
                 | CType (Maybe Dest) Comp (Maybe Jump)
                 | Macro [Instruction]
                 deriving (Show)

data Dest = A  | M  | D
          | AM | AD
          | MD
          | AMD
          deriving (Show)

data Jump = JMP | JEQ | JNE | JLT | JGT | JLE | JGE
          deriving (Show)

data Comp = Const
          | Echo  Reg
          | Neg   Reg
          | Inc   Reg
          | Dec   Reg
          | Arith Reg Op Reg
          | Not   Reg
          deriving (Show)

data Const = Zero | One | NegOne
           deriving (Show)

data Reg = RegA | RegM | RegD
         deriving (Show)
         
data Op = Add | Sub | And | Or
        deriving (Show)

--A Type numbers can only be 15 bits wide
isATypeNumInRange num
    | num > 65535 || num < 0 = False
    | otherwise              = True