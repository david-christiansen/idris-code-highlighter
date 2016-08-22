module Highlight.Parser

import Highlight.Regions

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

||| The data format output by Idris's highlighting routines
public export
data SExpr = SSym String | SString String | SInt Integer | SList (List SExpr)

%name SExpr sexp

export
Show SExpr where
  show (SSym x) = ":" ++ x
  show (SString x) = show x
  show (SInt x) = show x
  show (SList xs) = "(" ++ concat (intersperse " " (map show xs)) ++ ")"

sSymInj : SSym x = SSym y -> x = y
sSymInj Refl = Refl

sStringInj : SString x = SString y -> x = y
sStringInj Refl = Refl

sIntInj : SInt x = SInt y -> x = y
sIntInj Refl = Refl

sListInj : SList xs = SList ys -> xs = ys
sListInj Refl = Refl

DecEq SExpr where
  decEq (SSym x) (SSym y) with (decEq x y)
    decEq (SSym x) (SSym x) | Yes Refl  = Yes Refl
    decEq (SSym x) (SSym y) | No contra = No $ contra . sSymInj
  decEq (SSym x) (SString y) = No $ \(Refl) impossible
  decEq (SSym x) (SInt y) = No $ \(Refl) impossible
  decEq (SSym x) (SList xs) = No $ \(Refl) impossible
  decEq (SString x) (SSym y) = No $ \(Refl) impossible
  decEq (SString x) (SString y) with (decEq x y)
    decEq (SString x) (SString x) | Yes Refl  = Yes Refl
    decEq (SString x) (SString y) | No contra = No $ contra . sStringInj
  decEq (SString x) (SInt y) = No $ \(Refl) impossible
  decEq (SString x) (SList xs) = No $ \(Refl) impossible
  decEq (SInt x) (SSym y) = No $ \(Refl) impossible
  decEq (SInt x) (SString y) = No $ \(Refl) impossible
  decEq (SInt x) (SInt y) with (decEq x y)
    decEq (SInt x) (SInt x) | Yes Refl  = Yes Refl
    decEq (SInt x) (SInt y) | No contra = No $ contra . sIntInj
  decEq (SInt x) (SList xs) = No $ \(Refl) impossible
  decEq (SList xs) (SSym x) = No $ \(Refl) impossible
  decEq (SList xs) (SString x) = No $ \(Refl) impossible
  decEq (SList xs) (SInt x) = No $ \(Refl) impossible
  decEq (SList xs) (SList ys) with (assert_total $ decEq xs ys)
    decEq (SList xs) (SList xs) | Yes Refl  = Yes Refl
    decEq (SList xs) (SList ys) | No contra = No $ contra . sListInj

namespace Assoc
  ||| Because much of the highlighting information is in alist form,
  ||| we need a representation that lets us do lookups in alists. This
  ||| datatype represents the results of those lookups.
  |||
  ||| This datatype is slightly deficient - it should guarantee that
  ||| we find the _first_ matching key, but it only guarantees that we
  ||| find _a_ matching key. Ah well, duplicate keys don't occur in
  ||| Idris's highlighting output.
  |||
  ||| @ needle what we are looking for
  ||| @ value the associated information, once found
  ||| @ haystack where we are looking
  data Assoc : (needle : SExpr) -> (value : List SExpr) -> (haystack : List SExpr) -> Type where
    ||| The key was found at the beginning
    Car : Assoc k v (SList (k :: v) :: sexprs)
    ||| The key was found in the tail
    Cdr : Assoc k v sexprs -> Assoc k v (sexpr :: sexprs)

  total
  noAssocEmpty : Assoc needle value [] -> Void
  noAssocEmpty Car impossible
  noAssocEmpty (Cdr prf) impossible

  total
  assocConsNotList : Assoc needle value (headSexpr :: sexprs) ->
                     Not (xs : List SExpr ** headSexpr = SList xs) ->
                     Assoc needle value sexprs
  assocConsNotList (Cdr x) notEqList = x
  assocConsNotList Car nope = absurd $ nope (_ ** Refl)

  total
  assocNonEmpty : Assoc needle value (SList [] :: sexprs) -> Assoc needle value sexprs
  assocNonEmpty (Cdr x) = x

  -- The following function could probably be cleaned up a bit, but that would require real thought...
  ||| Do a lookup in an alist.
  total
  assoc : (needle : SExpr) -> (haystack : List SExpr) -> Dec (value : List SExpr ** Assoc needle value haystack)
  assoc needle [] = No $ \(v ** prf) => noAssocEmpty prf
  assoc needle (SSym x :: xs) with (assoc needle xs)
    assoc needle (SSym x :: xs) | Yes prf   = Yes (fst prf ** Cdr (snd prf))
    assoc needle (SSym x :: xs) | No contra =
      No $ \(value ** location) =>
        contra (value ** assocConsNotList location (\(xs ** Refl) impossible))
  assoc needle (SString x :: xs) with (assoc needle xs)
    assoc needle (SString x :: xs) | Yes prf   = Yes (fst prf ** Cdr (snd prf))
    assoc needle (SString x :: xs) | No contra =
      No $ \(value ** location) =>
        contra (value ** assocConsNotList location (\(xs ** Refl) impossible))
  assoc needle (SInt x :: xs) with (assoc needle xs)
    assoc needle (SInt x :: xs) | Yes prf   = Yes (fst prf ** Cdr (snd prf))
    assoc needle (SInt x :: xs) | No contra =
      No $ \(value ** location) =>
        contra (value ** assocConsNotList location (\(xs ** Refl) impossible))
  assoc needle (SList [] :: xs) with (assoc needle xs)
    assoc needle (SList [] :: xs) | Yes prf   = Yes (fst prf ** Cdr (snd prf))
    assoc needle (SList [] :: xs) | No contra =
      No $ \(value ** location) =>
        contra (value ** assocNonEmpty location)
  assoc needle (SList (y :: ys) :: xs) with (decEq needle y)
    assoc needle (SList (needle :: ys) :: xs) | Yes Refl = Yes (_ ** Car)
    assoc needle (SList (y :: ys) :: xs) | No notHere with (assoc needle xs)
      assoc needle (SList (y :: ys) :: xs) | No notHere | Yes prf = Yes (fst prf ** Cdr (snd prf))
      assoc needle (SList (y :: ys) :: xs) | No notHere | No notThere =
        No $ \(value ** location) =>
          case location of
            Car => notHere Refl
            Cdr tl => notThere (_ ** tl)

||| Parse a parenthesized sequence
inParens : Parser a -> Parser a
inParens p = token "(" *!> p <* token ")"

||| Parse the end of an escape sequence in a string
escaped : Parser Char
escaped = char '\\' <|>| char '"' -- TODO - more?

||| Parse a single character from a string (possibly an escape sequence)
stringChar : Parser Char
stringChar = (char '\\' *!> escaped) <|>| (satisfy (\c => c /= '\\' && c /= '"'))

||| Parse a string literal
stringLit : Parser String
stringLit = lexeme $ char '"' *!> (pack <$> many stringChar) <* char '"'

||| Parse a character that is allowed as part of a symbol name
symbolChar : Parser Char
symbolChar = satisfy (\c => ('a' <= c && c <= 'z') ||
                            ('A' <= c && c <= 'Z') ||
                            ('0' <= c && c <= '9') ||
                            (c == '-'))

||| Parse a symbol
symbol : Parser String
symbol = lexeme $ char ':' *> (pack <$> many symbolChar)

||| Parse a whole S-expression
export
expr : Parser SExpr
expr =      SString <$> stringLit
       <|>| SSym <$> symbol
       <|>| SInt <$> lexeme integer
       <|>| SList <$> inParens (many expr)


decToMaybe : Dec a -> Maybe a
decToMaybe (Yes p) = Just p
decToMaybe (No _) = Nothing

total
getRegion : SExpr -> Maybe (Region ())
getRegion (SList xs) = do ([SString fn] ** _) <- decToMaybe (assoc (SSym "filename") xs)
                            | _ => Nothing
                          ([SInt sl, SInt sc] ** _) <- decToMaybe (assoc (SSym "start") xs)
                            | _ => Nothing
                          ([SInt el, SInt ec] ** _) <- decToMaybe (assoc (SSym "end") xs)
                            | _ => Nothing
                          return (MkRegion fn sl sc el ec ())
getRegion _ = Nothing

export
getRegionMeta : SExpr -> Maybe (Region SExpr)
getRegionMeta (SList [r, meta]) = map (const meta) <$> getRegion r
getRegionMeta _ = Nothing

getHighlightType : SExpr -> Maybe HighlightType
getHighlightType (SList xs) = do ([SSym d] ** _) <- decToMaybe (assoc (SSym "decor") xs)
                                   | _ => Nothing
                                 case d of
                                   "bound" => Just (Bound False) -- TODO look for implicit
                                   "keyword" => Just Keyword
                                   other =>
                                     [| Name (isName other) (pure $ getDocstring xs) (pure $ getType xs) |]
  where
    isName : String -> Maybe NameHL
    isName "function" = Just Function
    isName "data"     = Just Constructor
    isName "type"     = Just TypeConstructor
    isName _          = Nothing
    getDocstring : List SExpr -> String
    getDocstring xs = case decToMaybe $ assoc (SSym "doc-overview") xs of
                        Just ([SString d] ** _) => d
                        _ => ""
    getType : List SExpr -> String
    getType xs = case decToMaybe $ assoc (SSym "type") xs of
                        Just ([SString d] ** _) => d
                        _ => ""
getHighlightType _ = Nothing

export
mkHls : List (Region SExpr) -> List (Region HighlightType)
mkHls [] = []
mkHls (r::rs) = case getHighlightType (metadata r) of
                  Nothing => mkHls rs
                  Just hl => map (const hl) r :: mkHls rs
