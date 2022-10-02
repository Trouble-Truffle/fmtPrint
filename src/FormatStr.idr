import Data.List
import Data.List1
import Data.String
import Data.String.Extra
import Data.Morphisms
import Data.Maybe
import Data.Nat
import Data.Bits

data PrintFT = PFTInt PrintFT
             | PFTUInt PrintFT
             | PFTDecim Nat PrintFT
             | PFTSci Nat PrintFT
             | PFTFrac  PrintFT
             | PFTChar PrintFT
             | PFTBinary  PrintFT
             | PFTHex PrintFT
             | PFTHexUpper PrintFT
             | PFTString PrintFT
             | PFTEnd (List Char)

Show PrintFT where
  show (PFTInt f)      = "(PFTInt \{show f})"
  show (PFTUInt f)     = "(PFTUInt \{show f})"
  show (PFTDecim n f)  = "(PFTDecim \{show n} \{show f})"
  show (PFTFrac f)     = "(PFTFrac \{show f})"
  show (PFTChar f)     = "(PFTChar \{show f})"
  show (PFTBinary f)   = "(PFTBinary \{show f})"
  show (PFTHex f)      = "(PFTHex \{show f})"
  show (PFTHexUpper f) = "(PFTHexUpper \{show f})"
  show (PFTString f)   = "(PFTString \{show f})"
  show (PFTSci n f)      = "(PFTSci \{show f} \{show n})"
  show (PFTEnd xs)     = "(PFTEnd \{pack xs})"

printFt : List Char -> Maybe PrintFT
printFt ('f'::'.' :: xs) = let (l,r) = span isDigit xs
                           in PFTDecim !(parsePositive $ pack l) <$> (printFt xs)
printFt ('f'::'f'::xs) = PFTFrac <$> printFt xs
printFt ('f'::xs) = PFTDecim 5 <$> printFt xs
printFt ('i'::xs) = PFTInt <$> printFt xs 
printFt ('d'::xs) = PFTInt <$> printFt xs
printFt ('e'::'.' :: xs) = let (l,r) = span isDigit xs
                           in PFTSci !(parsePositive $ pack l) <$> (printFt xs)
printFt ('e'::xs) = PFTSci 5 <$> printFt xs
printFt ('s'::xs) = PFTString <$> printFt xs
printFt ('u'::xs) = PFTUInt <$> printFt xs
printFt ('x'::xs) = PFTHex <$> printFt xs
printFt ('X'::xs) = PFTHexUpper <$> printFt xs
printFt ('c'::xs) = PFTChar <$> printFt xs
printFt xs = Just $ PFTEnd xs

getEnd : PrintFT -> List Char
getEnd (PFTEnd xs) = xs
getEnd (PFTInt f) = getEnd f
getEnd (PFTUInt f) = getEnd f
getEnd (PFTDecim _ f) = getEnd f
getEnd (PFTFrac f) = getEnd f
getEnd (PFTChar f) = getEnd f
getEnd (PFTBinary f) = getEnd f
getEnd (PFTHex f) = getEnd f
getEnd (PFTHexUpper f) = getEnd f
getEnd (PFTString f) = getEnd f
getEnd (PFTSci _ f) = getEnd f

0 formatStrToType : PrintFT -> Type
formatStrToType (PFTInt f) = forall a . Show a => Num a => Neg a => a -> formatStrToType f
formatStrToType (PFTUInt f) = forall a . Show a => Num a => a -> formatStrToType f
formatStrToType (PFTDecim _ f) = forall a . Show a => Fractional a => a -> formatStrToType f
formatStrToType (PFTFrac f) = Double -> formatStrToType f
formatStrToType (PFTChar f) = forall a . Integral a => Cast a Char => a -> formatStrToType f
formatStrToType (PFTBinary f) = forall a . Cast a Integer => a -> formatStrToType f
formatStrToType (PFTHex f) = forall a . Ord a => Integral a => Cast a Char => a -> formatStrToType f
formatStrToType (PFTHexUpper f) = forall a . Ord a => Integral a => Cast a Char => a -> formatStrToType f
formatStrToType (PFTString f) = String -> formatStrToType f
formatStrToType (PFTSci _ f) = forall a . Show a => Num a => Neg a => a -> formatStrToType f
formatStrToType (PFTEnd _) = String

roundStr : Nat -> List Char -> List Char
roundStr n xs = let (l,r) = bimap id (fromMaybe [] . tail') $ span (/='.') xs 
                    (decimL,decimR) = splitAt n r
                    roundUp = maybe False ('5'<=) $ head' decimR
                    roundedVal = uncurry Data.List.snoc
                               . bimap id (if roundUp then advanceDigit else id) {d=Char}
                               . unsnoc 
                              <$> toList1' decimL
                in l ++ maybe [] (['.']++) roundedVal
                where
                  advanceDigit : Char -> Char
                  advanceDigit '9' = '0'
                  advanceDigit x = prim__cast_IntChar (prim__cast_CharInt x + 1)

infixl 4 <.>
(<.>) : (a -> b -> c) -> (a -> b) -> a -> c
f <.> g = (Mor f <*> Mor g).applyMor

round : Double -> Double
round x = if (0.5<=) $ ((-) <.> floor) x
             then ceiling x
             else floor x

eulerGCD : Integer -> Integer -> Integer
eulerGCD x 0 = x
eulerGCD 0 y = y
eulerGCD x y = if x < y 
                  then eulerGCD x (y `mod` x)
                  else eulerGCD y (x `mod` y)

toFraction : Double -> (Int,Int)
toFraction x = let integral = floor x
                   frac = x - integral
                   precision = 1000000
                   gcdVal = eulerGCD 
                              (cast $ round $ frac * precision)
                              (cast precision)
            in (cast $ round (frac * precision) / cast gcdVal
               ,cast $ precision / cast gcdVal)

doubleToDigits : Double -> (List Int, List Int)
doubleToDigits = ?todo_toDigits
  where
    digits = 53
    radix = 2
    range : (Int,Int)
    range = (-1021,1024)

--numToSci : Double -> (Integer, Nat)
--numToSci x = ?todo_sciNot
  --where
    --digits = 53
    --radix = 2

showHex : Ord a => Integral a => Cast a Char => a -> String
showHex x = 
  if x == 0 
    then "0"
    else let temp = x `mod` 16 
         in showHex (x `div` 16)
            `strSnoc`
            (if temp <= 9 then cast {to=Char} $ 48 + temp
                          else cast {to=Char} $ 87 + temp)


showBits : Cast a Integer => a -> String
showBits x = if cast x /= 0 
                then showBits (shiftR (cast x) 1) {a=Integer}
                     `strSnoc`
                     (if testBit {a=Integer} (cast x) 0 then '1' else '0')
                else ""

formatStrF : (format : PrintFT) -> String -> formatStrToType format
formatStrF (PFTInt f) s = formatStrF f . (s++) . show
formatStrF (PFTUInt f) s = formatStrF f . (s++) . show
formatStrF (PFTDecim n f) s = formatStrF f 
                            . (s++) 
                            . pack 
                            . roundStr n 
                            . unpack 
                            . show
formatStrF (PFTFrac f) s = formatStrF f 
                         . (s++) 
                         . (\(l,r) => "\{show l}/{show r}")
                         . toFraction
formatStrF (PFTChar f) s = formatStrF f 
                         . strSnoc s
                         . cast 
formatStrF (PFTBinary f) s = formatStrF f 
                           . (s++)
                           . (`strSnoc` 'b')
                           . showBits
formatStrF (PFTHex f) s = formatStrF f 
                        . (s++)
                        . ('#' `strCons`)
                        . showHex
formatStrF (PFTHexUpper f) s = formatStrF f
                             . (s++)
                             . ('#' `strCons`)
                             . toUpper
                             . showHex
formatStrF (PFTString f) s = formatStrF f . (s++)
formatStrF (PFTSci _ _) _ = ?TODO_Display_with_scientific_notation
formatStrF (PFTEnd _) s = s
