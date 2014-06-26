module CLTerm(
  assignType,
  baseCombinators,
  val, com, ap,
  var, con, func, prod,
  int, float, char) where

import Data.Map as M
import ErrorHandling

data CLTerm a = Val a | Com String | Ap (CLTerm a) (CLTerm a)
                                deriving (Eq, Ord)
                                         
instance Show a => Show (CLTerm a) where
  show (Val a) = show a
  show (Com name) = name
  show (Ap l r) = "(" ++ show l ++ show r ++ ")"
  
val = Val
com = Com
ap = Ap
  
data Type = Var String | Con String | Func Type Type | Prod Type Type
                                                       deriving (Eq, Ord)
                                                                
instance Show Type where
  show (Var name) = name
  show (Con name) = name
  show (Func l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (Prod l r) = "(" ++ show l ++ " x " ++ show r ++ ")"
  
var = Var
con = Con
func = Func
prod = Prod

class Typeable a where
  getType :: a -> Type
  
assignType :: (Typeable a) => Map String Type -> CLTerm a -> Error Type
assignType comTypes term = Failed "NOO"

baseCombinators =
  M.fromList [("I", func (var "o") (var "o")),
              ("K", func (var "o") (func (var "t") (var "o"))),
              ("S", func
                    (func (var "p") (func (var "o") (var "t")))
                    (func (func (var "p") (var "o")) (func (var "p") (var "t"))))]                      

              
-- Basic datatypes for language

data BasicT = INT Int | FLOAT Float | CHAR Char
                                      deriving (Eq, Ord, Show)

instance Typeable BasicT where
  getType (INT _) = Con "INT"
  getType (FLOAT _) = Con "FLOAT"
  getType (CHAR _) = Con "CHAR"
  
int = INT
float = FLOAT
char = CHAR