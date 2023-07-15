module Printer (printDatum, printDatums) where

-- GHC
import Data.List (intersperse)

-- ./prov
import Bird.Printer

-- ./src
import Datum

printDatum :: Datum -> String
printDatum = pretty 80 . ppd

printDatums :: [Datum] -> String
printDatums = pretty 80 . sep . map ppd

ppd :: Datum -> Doc
ppd (Symbol s) = text s
ppd (Const True) = text "#t"
ppd (Const False) = text "#f"
ppd (Num i) = text (show i)
ppd Nil = text "()"
ppd (PrimOp s _)  = text ("<PrimOp: " ++ s ++ ">")
ppd (Lambda _)  = text ("<lambda>")
ppd d@(Cons _ _) =
    case e of
        Nothing -> group (parens (sep (map ppd ds)))
        Just e' -> group (parens (sep (map ppd ds ++ [text ".", ppd e'])))
    where (ds, e) = comb d

comb :: Datum -> ([Datum], Maybe Datum)
comb (Cons d e) = (d : ds, e')
    where (ds, e') = comb e
comb Nil = ([], Nothing)
comb e = ([], Just e)

parens :: Doc -> Doc
parens d = text "(" <> d <> text ")"

sep :: [Doc] -> Doc
sep ds = mconcat (intersperse newline ds)
