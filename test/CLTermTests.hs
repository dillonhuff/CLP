module CLTermTests(
  allCLTermTests) where

import CLTerm
import ErrorHandling
import TestUtils

allCLTermTests = do
  assignTypeTests
  
assignTypeTests =
  testFunction (assignType baseCombinators) assignTypeCases
  
assignTypeCases =
  [(val $ int 4, Succeeded $ con "INT")]