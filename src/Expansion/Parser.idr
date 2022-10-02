import Expansion.Lexer 
import Data.Maybe
import Data.String
import Text.Lexer
import Text.Parser


data Expansion = Substitution String String 
               | GlobalSubstitution String String
               | Slice Int Int 
               | Index Int
               | RemovePrefix String
               | RemoveSuffix String
               | ReplacePrefix String String
               | ReplaceSuffix String String
               | Length
               | LowerCaseFirst
               | LowerCaseAll
               | UpperCaseFirst
               | UpperCaseAll

Show Expansion where
  show (Substitution a b) = "Substitution \{a} \{b}"
  show (GlobalSubstitution a b) = "GlobalSubstitution \{a} \{b}"
  show (Slice l r) = "Slice \{show l} \{show r}"
  show (Index i) = "Index \{show i}"
  show (RemovePrefix p) = "RemovePrefix \{p}"
  show (RemoveSuffix s) = "RemoveSuffix \{s}"
  show (ReplacePrefix a b) = "ReplacePrefix \{a} \{b}"
  show (ReplaceSuffix a b) = "ReplaceSuffix \{a} \{b}"
  show Length = "Length"
  show LowerCaseFirst = "LowerCaseFirst"
  show LowerCaseAll = "LowerCaseAll"
  show UpperCaseFirst = "UpperCaseFirst"
  show UpperCaseAll = "UpperCaseAll"

Parser' : Bool -> Type -> Type
Parser' = Grammar () ExpansSym 

Parser : Type -> Type
Parser = Grammar () ExpansSym True


acceptIf : ExpansSym -> String -> Parser ()
acceptIf sym txt = terminal txt 
                 $ \x => if x == sym 
                          then Just () 
                          else Nothing
colonP : Parser ()
colonP = acceptIf EXPColon "Expected ':'"

slashP : Parser ()
slashP = acceptIf EXPSlash "Expected '/'"

commaP : Parser ()
commaP = acceptIf EXPComma "Expected ','"

karatP : Parser ()
karatP = acceptIf EXPKarat "Expected '^'"

percentP : Parser ()
percentP = acceptIf EXPPercent "Expected '%'"

hashP : Parser ()
hashP = acceptIf EXPHash "Expected '#'"

dotP : Parser ()
dotP = acceptIf EXPDot "Expected '.'"

asteriskP : Parser ()
asteriskP = acceptIf EXPAsterisk "Expected '*'"

lBracketP : Parser ()
lBracketP = acceptIf EXPLBracket "Expected '['"

rBracketP : Parser ()
rBracketP = acceptIf EXPRBracket "Expected ']'"

closingP : Parser String
closingP = terminal "Expected '}'"
         $ \case
            EXPClosing s => Just s
            _ => Nothing

charP : Parser Char
charP = terminal "Expected CHAR" 
      $ \case 
        EXPChar c => Just c
        _ => Nothing

space : Parser ()
space = terminal "Expected WHITESPACE" 
      $ \case 
          EXPChar c => if isSpace c then Just () else Nothing
          _ => Nothing

stringP : Parser' False String
stringP = pack . toList <$> many anyCharP
  where
    anyCharP = terminal "Expected NON-SYNTAX-CHAR"
          $ \case
            EXPChar c => Just c
            _ => Nothing

numP : Parser' False Int
numP = stringP >>= maybe (fail "Expected NUMBER") 
                         Text.Parser.Core.pure 
                   . parseInteger

lexeme : {t : _} -> Parser' t a -> Parser' (t || Delay False) a
lexeme =  (<* many space)

substitutionP : Parser Expansion
substitutionP = Substitution <$> (stringP <* slashP) <*> stringP

globalSubstitutionP : Parser Expansion
globalSubstitutionP = GlobalSubstitution <$> (stringP <* slashP) <*> stringP

sliceP : Parser Expansion
sliceP = Slice <$> ((lexeme numP <|> pure 0) <* lexeme colonP) <*> lexeme numP

indexP : Parser Expansion
indexP = Index <$> (lexeme lBracketP *> lexeme numP <* lexeme rBracketP)

removePrefixP : Parser Expansion 
removePrefixP = RemovePrefix <$> (hashP *> stringP)

removeSuffixP : Parser Expansion
removeSuffixP = RemoveSuffix <$> (percentP *> stringP)

replacePrefixP : Parser Expansion 
replacePrefixP = ReplacePrefix <$> (hashP *> stringP) <*> (slashP *> stringP)

replaceSuffixP : Parser Expansion
replaceSuffixP = ReplaceSuffix <$> (percentP *> stringP) <*> (slashP *> stringP)

lowerCaseFirstP : Parser Expansion
lowerCaseFirstP = LowerCaseFirst <$ commaP

lowerCaseAllP : Parser Expansion
lowerCaseAllP = LowerCaseFirst <$ (commaP <* commaP)

upperCaseFirst : Parser Expansion
upperCaseFirst = UpperCaseFirst <$ karatP

upperCaseAll : Parser Expansion
upperCaseAll = UpperCaseAll <$ (karatP <* karatP)

lengthP : Parser Expansion
lengthP = Length <$ lexeme hashP

expressionP : Parser (Expansion, String)
expressionP = (,) <$>
            (  lengthP 
             <|> sliceP 
             <|> substitutionP 
             <|> globalSubstitutionP
             <|> sliceP
             <|> indexP
             <|> removeSuffixP
             <|> removePrefixP
             <|> replacePrefixP
             <|> replaceSuffixP
             <|> lowerCaseFirstP 
             <|> lowerCaseAllP 
             <|> upperCaseFirst
             <|> upperCaseAll
             ) <*> closingP
