import Expansion.Parser
import Data.List
import Data.String
import Data.So
import Data.String.Extra
import Data.List1

0 expansionToType : Expansion -> Type
expansionToType (Substitution _ _) = String -> String
expansionToType (GlobalSubstitution _ _) = String -> String
expansionToType (Slice _ _) = forall t, elem . Foldable t => t elem -> Maybe (List elem)
expansionToType (Index _) = forall t, elem . Foldable t => t elem -> Maybe elem
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


diff : (minuend : Nat) -> (subtrahend : Nat) -> {auto 0 _ : So (subtrahend <= minuend)} -> Nat
diff x y with (choose (S y <= x))
  _ | Left _ = S (x `diff` S y)
  _ | Right _ = Z

slice : (l : Nat) -> (r : Nat) -> (xs : List a) 
        -> { auto 0 _ : InBounds l xs}
        -> { auto 0 _ : InBounds r xs}
        -> { auto 0 _ : So (l <= r)}
        -> List a
slice l r xs = take (r `diff` l) $ drop l xs

slice' : Nat -> Nat -> List a -> Maybe (List a)
slice' l r xs with (choose $ l <= r) 
  _ | Right _ = Nothing
  _ | Left _ = Just $ take (r `diff` l) $ drop l xs

maybeIndex : Nat -> List a -> Maybe a
maybeIndex Z (x::xs) = Just x
maybeIndex (S n) (x::xs) = maybeIndex n xs
maybeIndex _ [] = Nothing

replacePrefix : String -> Lazy String -> String -> String
replacePrefix from to str = 
  if from `isPrefixOf` str
    then to ++ drop (length from) str
    else str

replaceSuffix : String -> Lazy String -> String -> String
replaceSuffix from to str = 
  if from `isSuffixOf` str 
     then take lenDiff str ++ to
     else str
  where
    lenDiff : Lazy Nat
    lenDiff = let sourceLen = length from
                  strLen = length str
              in case choose (sourceLen <= strLen) of
                   Left _ => strLen `diff` sourceLen
                   Right _ => 0

expansionToFunc : (e : Expansion) -> (expansionToType e)
expansionToFunc (Substitution from to) = substitute from to
expansionToFunc (GlobalSubstitution from to) = substituteAll from to
expansionToFunc (Slice l r) = slice' l r . toList
expansionToFunc (Index i) = maybeIndex i . toList
expansionToFunc (RemovePrefix from) = replacePrefix from ""
expansionToFunc (RemoveSuffix from) = replaceSuffix from ""
expansionToFunc (ReplacePrefix from to) = replacePrefix from to
expansionToFunc (ReplaceSuffix from to) = replaceSuffix from to
expansionToFunc Length = length . toList
expansionToFunc LowerCaseFirst = maybe "" (uncurry strCons . (mapFst toLower)) . strUncons
expansionToFunc LowerCaseAll = toLower
expansionToFunc UpperCaseFirst = maybe "" (uncurry strCons . (mapFst toUpper)) . strUncons
expansionToFunc UpperCaseAll = toUpper
