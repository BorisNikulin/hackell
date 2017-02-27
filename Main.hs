data Instruction = AType Int
				 | CType (Maybe Dest) Comp (Maybe Jump)
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