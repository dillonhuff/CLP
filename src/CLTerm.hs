module CLTerm(
  assignType,
  baseCombinators,
  val, com, CLTerm.ap,
  var, con, func, base,
  int, float, char) where

import Control.Monad hiding (ap)
import Data.List as L
import Data.Map as M
import Data.Set as S
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
  
data Type = Var String | Base String | Con String Int [Type]
                                                       deriving (Ord)
                                                                
instance Eq Type where
  (==) (Var n1) (Var n2) = n1 == n2
  (==) (Base n1) (Base n2) = n1 == n2
  (==) (Con n1 a1 _) (Con n2 a2 _) = n1 == n2 && a1 == a2
  (==) _ _ = False

instance Show Type where
  show (Var name) = name
  show (Base name) = name
  show (Con "->" 2 [x, y]) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (Con n _ params) = n ++ " " ++ (L.concat $ L.intersperse " " $ L.map show params)
  
var = Var
con = Con
base = Base

func = Con "->" 2

allVars :: Type -> [Type]
allVars (Base _) = []
allVars (Var n) = [Var n]
allVars (Con _ _ params) = L.nub $ L.concat $ L.map allVars params

class Typeable a where
  getType :: a -> Type
  
assignType :: (Typeable a) => Map String Type -> CLTerm a -> Error Type
assignType comTypes term = failOrType
  where
    rootTypeVar = "t0"
    constraints = typeConstraints comTypes rootTypeVar term
    unifier = constraints >>= unify
    failOrType = liftM (\sub -> prettyTypeVars $ applySubstitution sub (var rootTypeVar)) unifier
    
prettyTypeVars :: Type -> Type
prettyTypeVars t = applySubstitution uglyVarsToPrettyVars t
  where
    varsInType = allVars t
    prettyVars = take (length varsInType) (L.map var (L.map (:[]) ['a'..'z']))
    uglyVarsToPrettyVars = case length varsInType > 25 of
      True -> error $ "MORE THAN 25 VARS IN TYPE EXPRESSION"
      False -> M.fromList $ zip varsInType prettyVars

baseCombinators =
  M.fromList [("I", func [var "o", var "o"]),
              ("K", func [var "o", func [var "t", var "o"]]),
              ("S", func
                    [func [var "p", func [var "o", var "t"]],
                     func [func [var "p", var "o"], func [var "p", var "t"]]])]

              
-- Basic datatypes for language

data BasicT = INT Int | FLOAT Float | CHAR Char
                                      deriving (Eq, Ord, Show)

instance Typeable BasicT where
  getType (INT _) = Base "INT"
  getType (FLOAT _) = Base "FLOAT"
  getType (CHAR _) = Base "CHAR"
  
int = INT
float = FLOAT
char = CHAR

-- Type checking
type TypeConstraint = (Type, Type)
type TypeSubstitution = Map Type Type

typeConstraint :: Type -> Type -> TypeConstraint
typeConstraint t1 t2 = (t1, t2)

replaceSub :: Type -> Type -> TypeSubstitution -> TypeSubstitution
replaceSub t1 t2 sub = M.mapKeys (replaceBy t1 t2) (M.map (replaceBy t1 t2) sub)

replaceTC :: Type -> Type -> TypeConstraint -> TypeConstraint
replaceTC t1 t2 (x, y) = (replaceBy t1 t2 x, replaceBy t1 t2 y)

replaceBy :: Type -> Type -> Type -> Type
replaceBy t1 t2 x = case x == t1 of
  True -> t2
  False -> x

typeConstraints :: (Typeable a) => Map String Type -> String -> CLTerm a -> Error [TypeConstraint]
typeConstraints _ tv (Val v)  = Succeeded [typeConstraint (var tv) (getType v)]
typeConstraints comTypes tv (Com name) = case M.lookup name comTypes of
  Just t -> Succeeded [typeConstraint (var tv) (uniqueTypeVars tv t)]
  Nothing -> Failed $ "Unrecognized combinator " ++ name
typeConstraints comTypes tv (Ap l r) = liftM2 (++) (Succeeded thisTC) (liftM2 (++) leftTC rightTC)
  where
    leftTV = tv ++ "0"
    rightTV = tv ++ "1"
    thisTC = [typeConstraint (var leftTV) (func [var rightTV, var tv])]
    leftTC = typeConstraints comTypes leftTV l
    rightTC = typeConstraints comTypes rightTV r
  
uniqueTypeVars :: String -> Type -> Type
uniqueTypeVars uniquePrefix (Var n) = Var (n ++ uniquePrefix)
uniqueTypeVars uniquePrefix (Con n a params) = Con n a $ L.map (uniqueTypeVars uniquePrefix) params
uniqueTypeVars _ t = t

unify :: [TypeConstraint] -> Error TypeSubstitution
unify constraints = unf M.empty constraints

unf :: TypeSubstitution -> [TypeConstraint] -> Error TypeSubstitution
unf sub [] = Succeeded sub
unf sub ((Base n1, Base n2):tcs) = case n1 == n2 of
  True -> unf sub tcs
  False -> unificationError (Base n1) (Base n2)
unf sub ((t1@(Var n1), t2):tcs) = case t1 == t2 of
  True -> unf sub tcs
  False -> unf newSub newTCS
  where
    newSub = M.insert t1 t2 (replaceSub t1 t2 sub)
    newTCS = replaceInTCS t1 t2 tcs
unf sub ((t1, Var n):tcs) = unf sub ((Var n, t1):tcs)
unf sub ((Con n1 a1 p1, Con n2 a2 p2):tcs) = case (Con n1 a1 p1) == (Con n2 a2 p2) of
  True -> unf sub (zip p1 p2 ++ tcs)
  False -> unificationError (Con n1 a1 p1) (Con n2 a2 p2)
unf _ ((t1, t2):tcs) = unificationError t1 t2
    
replaceInTCS :: Type -> Type -> [TypeConstraint] -> [TypeConstraint]
replaceInTCS t1 t2 tcs = L.map (replaceTC t1 t2) tcs
  
unificationError :: Type -> Type -> Error TypeSubstitution
unificationError t1 t2 = Failed $ "Cannot unify types " ++ show t1 ++ " and " ++ show t2

applySubstitution :: TypeSubstitution -> Type -> Type
applySubstitution sub (Con name arity params) = Con name arity (L.map (applySubstitution sub) params)
applySubstitution sub v = case M.lookup v sub of
  Just s -> applySubstitution sub s
  Nothing -> v