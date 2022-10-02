import Expansion.Parser
import Data.List
import Data.String
import Data.So
import Data.String.Extra
import Data.List1

0 expansionToType : Expansion -> Type
expansionToType (Substitution _ _) = String -> String
expansionToType (GlobalSubstitution _ _) = String -> String
expansionToType (Slice _ _) = forall t, elem . Foldable t => t elem -> t elem
expansionToType (Index _) = forall t, elem . Foldable t => t elem -> elem
expansionToType (RemovePrefix _) = String -> String
expansionToType (RemoveSuffix _) = String -> String
expansionToType (ReplacePrefix _ _) = String -> String
expansionToType (ReplaceSuffix _ _) = String -> String
expansionToType Length = forall t, elem . Foldable t => t elem -> Nat
expansionToType LowerCaseFirst = String -> String
expansionToType LowerCaseAll = String -> String
expansionToType UpperCaseFirst = String -> String
expansionToType UpperCaseAll = String -> String

substitute : String -> String -> String -> String
substitute from to = replacePrefixMatch . strUncons
  where 
  replacePrefixMatch : Maybe (Char, String) -> String
  replacePrefixMatch Nothing = ""
  replacePrefixMatch (Just (x,xs)) =
    if from `isPrefixOf` (x `strCons` xs)
       then to ++ drop (length from) (x `strCons` xs)
       else strCons x $ replacePrefixMatch $ strUncons xs

substituteAll : String -> String -> String -> String
substituteAll from to = replacePrefixMatch . strUncons
  where 
  replacePrefixMatch : Maybe (Char, String) -> String
  replacePrefixMatch Nothing = ""
  replacePrefixMatch (Just (x,xs)) =
    if from `isPrefixOf` (x `strCons` xs)
       then to ++ replacePrefixMatch (strUncons $ drop (length from) (x `strCons` xs))
       else strCons x $ replacePrefixMatch $ strUncons xs

slice : (l : Nat) -> (r : Nat) -> (xs : List a) 
        -> { auto 0 _ : InBounds l xs}
        -> { auto 0 _ : InBounds r xs}
        -> { auto 0 _ : So (r <= l)}
        -> List a
--slice l r xs = take difference $ drop l xs
  --where 

diff : (x : Nat) -> (y : Nat) -> {auto 0 _ : So (x <= y)} -> Nat
diff x y with (choose (S x <= y))
  _ | Left _ = S (S x `diff` y)
  _ | Right _ = Z


expansionToFunc : (e : Expansion) -> (expansionToType e)
expansionToFunc (Substitution from to) = ?todo_sub
expansionToFunc (GlobalSubstitution _ _) = ?expansionToFunc_missing_case_1
expansionToFunc (Slice _ _) = ?expansionToFunc_missing_case_2
expansionToFunc (Index _) = ?expansionToFunc_missing_case_3
expansionToFunc (RemovePrefix _) = ?expansionToFunc_missing_case_4
expansionToFunc (RemoveSuffix _) = ?expansionToFunc_missing_case_5
expansionToFunc (ReplacePrefix _ _) = ?expansionToFunc_missing_case_6
expansionToFunc (ReplaceSuffix _ _) = ?expansionToFunc_missing_case_7
expansionToFunc Length = ?expansionToFunc_missing_case_8
expansionToFunc LowerCaseFirst = ?expansionToFunc_missing_case_9
expansionToFunc LowerCaseAll = ?expansionToFunc_missing_case_10
expansionToFunc UpperCaseFirst = ?expansionToFunc_missing_case_11
expansionToFunc UpperCaseAll = ?expansionToFunc_missing_case_12
