import Data.Maybe
import Data.String
import Text.Lexer
import Text.Parser

interface PrintLnR a where 
  toPrintLnR : String -> a

data ExpansSym = EXPChar Char
               | EXPColon
               | EXPSlash
               | EXPComma
               | EXPKarat
               | EXPPercent
               | EXPHash
               | EXPDot
               | EXPLBracket
               | EXPRBracket
               | EXPAsterisk
               | EXPClosing String

Eq ExpansSym where
  EXPChar _ == EXPChar _ = True
  EXPClosing _ == EXPClosing _   = True
  EXPColon    == EXPColon     = True
  EXPSlash    == EXPSlash     = True
  EXPComma    == EXPComma     = True
  EXPKarat    == EXPKarat     = True
  EXPPercent  == EXPPercent   = True
  EXPHash     == EXPHash      = True
  EXPDot      == EXPDot       = True
  EXPAsterisk == EXPAsterisk  = True
  EXPLBracket == EXPLBracket  = True
  EXPRBracket == EXPRBracket  = True
  _ == _ = False

Show ExpansSym where
  show (EXPChar c) = "EXPChar \{show c}"
  show EXPColon = "EXPColon"
  show EXPSlash = "EXPSlash"
  show EXPComma = "EXPComma"
  show EXPKarat = "EXPKarat"
  show EXPPercent = "EXPPercent"
  show EXPHash = "EXPHash"
  show EXPDot = "EXPDot"
  show EXPAsterisk = "EXPAsterisk"
  show (EXPClosing s) = "EXPClosing \{s}"
  show EXPLBracket = "EXPLBracket "
  show EXPRBracket = "EXPRBracket"

expansLexer : TokenMap ExpansSym
expansLexer = [(is '\\' <+> any, EXPChar . maybe '\\' (fst . snd) 
                                         . (bitraverse Just strUncons <=< strUncons) )
              ,(is ':', const EXPColon)
              ,(is '/', const EXPSlash)
              ,(is ',', const EXPComma)
              ,(is '^', const EXPKarat)
              ,(is '%', const EXPPercent)
              ,(is '#', const EXPHash)
              ,(is '.', const EXPDot)
              ,(is '*', const EXPAsterisk)
              ,(is '[', const EXPLBracket)
              ,(is ']', const EXPRBracket)
              ,(is '}' <+> many any, EXPClosing)
              ,(any, \s => maybe (EXPClosing s) (EXPChar . fst) $ strUncons s )
              ]
