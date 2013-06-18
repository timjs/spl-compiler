{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Language.SPL.Data.Program where

import Language.SPL.Printer


-- Types -----------------------------------------------------------------------

type Program    = Constructs
type Constructs = [Construct]

data Construct = Declaration Type Name Expression
               | Definition  Type Name Parameters Constructs Statements
               deriving (Show, Eq, Ord)

data Type = VOID
          | INT
          | BOOL
          | PAIR Type Type
          | LIST Type
          | POLY String
          deriving (Show, Eq, Ord)
data Name = Globals
          | Main
          | Print
          | IsEmpty
          | Head
          | Tail
          | Fst
          | Snd
          | Name String
          | Temp String
          deriving (Show, Eq, Ord)

type Parameters = [Parameter]
data Parameter  = Parameter Type Name
                deriving (Show, Eq, Ord)

type Arguments  = [Argument]
type Argument   = Expression

type Statements = [Statement]
data Statement  = Assign  Name Expression
                | If      Expression Statements Statements
                | While   Expression Statements
                | Match   Name Cases
                | Return  (Maybe Expression)
                | Execute Name Arguments
                deriving (Show, Eq, Ord)

type Expressions = [Expression]
data Expression  = Value    Name
                 | Integer  Integer
                 | Boolean  Bool
                 | List
                 | Pair     Expression Expression
                 | Call     Name Arguments
                 | Infix    BinaryOperator Expression Expression
                 | Prefix   UnaryOperator Expression
                 deriving (Show, Eq, Ord)

type Cases   = [Case]
data Case    = Case Pattern Statements
             deriving (Show, Eq, Ord)
data Pattern = Anything
             | NamePattern Name
             | ListPattern
             | ConsPattern Pattern Pattern
             | PairPattern Pattern Pattern
             | IntPattern  Integer
             | BoolPattern Bool
             deriving (Show, Eq, Ord)

data BinaryOperator = Add | Sub | Mul | Div | Mod
                    | Eq | Ne | Lt | Gt | Le | Ge 
                    | And | Or | Cons
                    deriving (Show, Eq, Ord, Enum)
data UnaryOperator  = Not | Neg
                    deriving (Show, Eq, Ord, Enum)

type Arrity      = Int
type Signature   = [Type]

class HasName a where
  name :: a -> Name
instance HasName Construct where
  name (Declaration _ n _)    = n
  name (Definition _ n _ _ _) = n
instance HasName Parameter where
  name (Parameter _ n) = n

isDefinition :: Construct -> Bool
isDefinition Definition {} = True
isDefinition _             = False

isDeclaration :: Construct -> Bool
isDeclaration Declaration {} = True
isDeclaration _              = False

-- Pretty Printer --------------------------------------------------------------

instance Pretty Program where
  pretty = vsep . map pretty

instance Pretty Construct where
  pretty c = case c of
    Declaration t n e    -> pretty t <+> pretty n <+>
                            align (equals <+> pretty e) <> semi
    Definition t n p d b -> empty <$>
                            pretty t <+> pretty n <+> pretty p <+>
                            block (pretty d <$> pretty b)

instance Pretty Type where
  pretty t = case t of
    VOID     -> annotation "Void"
    INT      -> annotation "Int"
    BOOL     -> annotation "Bool"
    PAIR s t -> parensized [s,t]
    LIST t   -> brackets (pretty t)
    POLY s   -> annotation s

instance Pretty Name where
  pretty n = case n of
    Globals -> identifier "_globals"
    Main    -> identifier "main"
    Print   -> identifier "print"
    IsEmpty -> identifier "isEmpty"
    Head    -> identifier "head"
    Tail    -> identifier "tail"
    Fst     -> identifier "fst"
    Snd     -> identifier "snd"
    Name s  -> identifier s
    Temp s  -> identifier $ '_' : s

instance Pretty Parameters where
  pretty = parensized

instance Pretty Parameter where
  pretty (Parameter t n) = pretty t <+> pretty n

instance Pretty Arguments where
  pretty = parensized

instance Pretty Statements where
  pretty = vsep . map pretty

instance Pretty Statement where
  pretty s = case s of
    Assign n e  -> pretty n <+> equals <+> pretty e <> semi
    Execute n a -> pretty n <> pretty a <> semi
    Return m    -> keyword "return" <+> pretty m <> semi
    If c t e    -> keyword "if" <+> parens (pretty c) <+> 
                   block t <+>
                   keyword "else" <+>
                   block e
    While c l   -> keyword "while" <+> parens (pretty c) <+>
                   block l
    Match n cs  -> keyword "match" <+> pretty n <$>
                   pretty cs

instance Pretty Case where
  pretty c = case c of
    Case e ss -> keyword "case" <+> parens (pretty e) <+> block ss

instance Pretty Cases where
  pretty = vsep . map pretty

instance Pretty Pattern where
  pretty p = case p of
    Anything        -> constant '_'
    NamePattern n   -> identifier n
    ListPattern     -> constant "[]"
    ConsPattern l r -> pretty l <> operator ':' <> pretty r --TODO: parens ?
    PairPattern l r -> parensized [l,r]
    IntPattern  i   -> constant i
    BoolPattern b   -> constant b

instance Pretty Expression where
  pretty e = case e of
    Value n     -> pretty n
    Integer i   -> constant i
    Boolean b   -> constant b
    List        -> constant "[]"
    Pair x y    -> parensized [x,y]
    Call n as   -> pretty n <> pretty as
    Infix o l r -> parens . align $ pretty l <+> pretty o <+> pretty r
    Prefix o e  -> parens $ pretty o <> pretty e

instance Pretty BinaryOperator where
  pretty o = case o of
    Add  -> operator '+'
    Sub  -> operator '-'
    Mul  -> operator '*'
    Div  -> operator '/'
    Mod  -> operator '%'
    Eq   -> operator "=="
    Ne   -> operator "!->"
    Lt   -> operator '<'
    Gt   -> operator '>'
    Le   -> operator "<="
    Ge   -> operator ">="
    And  -> operator "&&"
    Or   -> operator "||"
    Cons -> operator ':'

instance Pretty UnaryOperator where
  pretty o = case o of
    Not -> operator '!'
    Neg -> operator '-'
