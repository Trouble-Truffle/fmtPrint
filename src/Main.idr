module Main

import Expansion
import FormatStr 
import Expansion.Lexer
import Expansion.Parser

import Data.Maybe
import Data.Either
import Data.String
import Text.Lexer
import Text.Parser


data Format = FShow Format
            | FPrintF PrintFT Format
            | FExpand Expansion Format
            | FOther Char Format
            | FEnd

Show Format where
  show (FShow f) = "(FShow \{show f})"
  show (FPrintF p f) = "(FPrintF \{show p} \{show f})"
  show (FExpand e f) = "(FPrintF \{show e} \{show f})"
  show (FOther c f) = "(FOther \{show c} \{show f})"
  show FEnd = "FEnd"

formatT : String -> Maybe Format
formatT = go . unpack
  where 
    go : List Char -> Maybe Format 
    go ('\\'::'{'::xs) = FOther '\\' . FOther '{' <$> go xs
    go ('{'::'}'::xs) = FShow <$> (go xs)
    go ('{'::xs) = let (l,r) = !(parseExp xs)
                    in FExpand l <$> go (unpack r)
      where
        parseExp = map fst 
                 . eitherToMaybe
                 . parse expressionP 
                 . fst 
                 . lex expansLexer 
                 . pack
    go ('%'::'%'::xs) = FOther '%' <$> go xs
    go ('%'::xs) = let format = !(printFt xs)
                   in FPrintF format <$> (go $ getEnd format)
    go (x::xs) = FOther x <$> go xs
    go [] = Just FEnd

0 typeF : Format -> Type
typeF (FShow f) = forall a . Show a => a -> typeF f
typeF (FPrintF e f) = formatStrToType e -> typeF f 
typeF (FExpand e f) = expansionToType e -> typeF f
typeF (FOther _ f) = typeF f
typeF FEnd = String

--formatToFunction : (f : Format) -> String -> (typeF f)
--formatToFunction (FShow f) s = formatToFunction f . (s++) . show
--formatToFunction (FPrintF e f) s = formatToFunction f . (s++) . formatStrF e
--formatToFunction (FExpand _ _) s = ?formatToFunction_missing_case_2
--formatToFunction (FOther _ _) s = ?formatToFunction_missing_case_3
--formatToFunction FEnd s = ?formatToFunction_missing_case_4

--formatLn : (s : String) 
--          -> { auto 0 _ : IsJust (typeF <$> formatT s) } 
--          -> { auto 0 _ : IsJust (formatT s) } 
--          -> fromJust (typeF <$> formatT s)
--formatLn s = let format' = fromJust $ formatT s
--  in ?todo_formatLn

main : IO ()
main = do 
  putStrLn "Hello from Idris2!"
