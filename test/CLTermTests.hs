module CLTermTests(
  allCLTermTests) where

import CLTerm
import Data.Map as M
import ErrorHandling
import TestUtils

allCLTermTests = do
  basicAssignTypeTests
  advancedAssignTypeTests
  
basicAssignTypeTests =
  testFunction (assignType baseCombinators) assignTypeCases
  
assignTypeCases =
  [(val $ int 4, Succeeded $ base "INT"),
   (com "I", Succeeded $ func [var "a", var "a"]),
   (ap (com "I") (val $ char 'a'), Succeeded $ base "CHAR"),
   (ap (com "K") (val $ float 12.2), Succeeded $ func [var "a", base "CHAR"]),
   (ap (com "K") (com "I"), Succeeded $ func [var "a", func [var "b", var "b"]]),
   (ap (ap (com "K") (com "I")) (val $ float (-1)), Succeeded $ func [var "a", var "a"]),
   (ap (ap (com "S") (com "K")) (com "K"), Succeeded $ func [var "a", var "a"]),
   (ap (ap (com "S") (ap (com "K") (com "S"))) (com "K"),
    Succeeded $ func [func [var "a", var "b"], func [func [var "c", var "a"], func [var "c", var "b"]]])]
  
advancedAssignTypeTests =
  testFunction (assignType extendedCombinators) advancedCases
  
advancedCases =
  [(ap (ap (com "Cons") (val $ int 4)) (com "Nil"), Succeeded $ con "List" 1 [var "a"]),
   (ap (ap (ap (com "Node") (val $ char 'a'))
        (ap (com "Leaf") (val $ int 4))) (ap (com "Leaf") (val $ int 2)),
    Succeeded $ con "Tree" 2 [base "CHAR", base "INT"]),
   (ap (com "Node") (val $ float 2.3),
    Succeeded $ func [con "Tree" 2 [base "FLOAT", var "a"],
                      func [con "Tree" 2 [base "FLOAT", var "a"],
                            con "Tree" 2 [base "FLOAT", var "a"]]])]
  
extendedCombinators = M.union baseCombinators advancedCombinators

advancedCombinators =
  M.fromList [("Cons", func [var "a", func [con "List" 1 [var "a"], con "List" 1 [var "a"]]]),
              ("Nil", con "List" 1 [var "a"]),
              ("Node", func [var "a",
                             func [con "Tree" 2 [var "a", var "b"],
                                   func [con "Tree" 2 [var "a", var "b"],
                                         con "Tree" 2 [var "a", var "b"]]]]),
              ("Leaf", func [var "b",
                             con "Tree" 2 [var "a", var "b"]])]