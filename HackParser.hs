import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Maybe

import Text.Parsec
import Text.Parsec.String

type Symbol = String

data Instruction = AType AVal
                 | CType (Maybe Dest) Comp (Maybe Jump)
                 | Macro [Instruction]
                 deriving (Show)

data AVal = ANum    Int
          | ASymbol Symbol
          deriving (Show, Read)

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

--parseHackLine = parseAType

parseAType :: Text.Parsec.String.Parser Instruction
parseAType = do
    char '@'
    atInst <- AType <$> (ANum <$> read <$> many1 digit <|> ASymbol <$> (letter `combin many alphaNum))
    spaces
    endOfLine
    return atInst

--A Type numbers can only be 15 bits wide
isATypeNumInRange num
    | num > 65535 || num < 0 = False
    | otherwise              = True