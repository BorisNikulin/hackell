import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Maybe

type Symbol = T.Text

data Instruction = AType (Either Int Symbol)
                 | CType (Maybe Dest) Comp (Maybe Jump)
                 | Macro [Instruction]
                 deriving (Show)

{-
data Dest = Dest { aDest :: Bool
                 , mDest :: Bool
                 , dDest :: Bool
                 } deriving (Show)
-}

--{-
data Dest = A  | M  | D
          | AM | AD
          | MD
          | AMD
          deriving (Show)
---}

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

--parse :: T.Text -> Either String Instruction
parse s = let s' = T.strip s
          in case T.uncons s' of
                  Just ('@', num) -> case T.decimal num of
                                          Right (num', t) -> if T.null t
                                                               then if isATypeNumInRange num'
                                                                      then Right (AType $ Left num')
                                                                      else Left "Number in A Type Instruction exceeding 15 bits"
                                                               else Left "Trailing non digit characters after A Type Instruction"


isATypeNumInRange num
    | num > 65535 || num < 0 = False
    | otherwise              = True