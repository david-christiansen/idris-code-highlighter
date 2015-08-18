module Highlight.Regions

||| There are three types of global names that we will highlight
data NameHL = Function | Constructor | TypeConstructor

instance Show NameHL where
  showPrec d Function = "Function"
  showPrec d Constructor = "Constructor"
  showPrec d TypeConstructor = "TypeConstructor"

instance Eq NameHL where
  Function == Function = True
  Function == Constructor = True
  TypeConstructor == TypeConstructor = True
  _ == _ = False

instance Ord NameHL where
  compare Function Function = EQ
  compare Function Constructor = LT
  compare Function TypeConstructor = LT
  compare Constructor Function = GT
  compare Constructor Constructor = EQ
  compare Constructor TypeConstructor = LT
  compare TypeConstructor Function = GT
  compare TypeConstructor Constructor = GT
  compare TypeConstructor TypeConstructor = EQ

||| Highlighting instructions
data HighlightType : Type where
  ||| This is a name from the global context
  Name : NameHL -> (docstring : String) -> (type : String) -> HighlightType
  ||| This is a bound name from the local context
  Bound : (isImplicit : Bool) -> HighlightType
  ||| This is a language keyword (or part of a user syntax rule)
  Keyword : HighlightType

instance Eq HighlightType where
  (Name hl doc ty) == (Name hl' doc' ty') = hl == hl' && doc == doc' && ty == ty'
  (Bound b) == (Bound b') = b == b'
  Keyword == Keyword = True
  _ == _ = False

instance Ord HighlightType where
  compare (Name x y z) (Name a b c) =
    case compare x a of
      LT => LT
      GT => GT
      EQ => case compare y b of
              LT => LT
              GT => GT
              EQ => compare z c
  compare (Name x y z) (Bound _) = LT
  compare (Name x y z) Keyword = LT
  compare (Bound _) (Name _ _ _) = GT
  compare (Bound x) (Bound y) = compare x y
  compare (Bound _) Keyword = LT
  compare Keyword (Name _ _ _) = GT
  compare Keyword (Bound _) = GT
  compare Keyword Keyword = EQ

instance Show HighlightType where
    showPrec d (Name x docstring type) =
        showCon d "Name" $ showArg x ++ showArg docstring ++ showArg type
    showPrec d (Bound isImplicit) =
        showCon d "Bound" $ showArg isImplicit
    showPrec d Keyword = "Keyword"

||| Regions are spans in the source code
record Region meta where
  constructor MkRegion
  fileName : String
  startLine : Integer
  startColumn : Integer
  endLine : Integer
  endColumn : Integer
  metadata : meta

instance Functor Region where
  map f (MkRegion fn sl sc el ec meta) =
    MkRegion fn sl sc el ec (f meta)

instance Show a => Show (Region a) where
  showPrec d x = showCon d "MkRegion" $
                   showArg (fileName x) ++
                   showArg (startLine x) ++ showArg (startColumn x) ++
                   showArg (endLine x) ++ showArg (endColumn x) ++
                   showArg (metadata x)

instance Eq a => Eq (Region a) where
  r1 == r2 = fileName r1 == fileName r2 &&
             startLine r1 == startLine r2 &&
             startColumn r1 == startColumn r2 &&
             endLine r1 == endLine r2 &&
             endColumn r1 == endColumn r2 &&
             metadata r1 == metadata r2

||| This Ord instance is important - we must sort regions in
||| increasing order of start position, then increasing order of end
||| position. That means we can traverse them in order for opening and
||| use a stack to track end positions.
instance Ord a => Ord (Region a) where
  compare r1 r2 =
      case compare (fileName r1) (fileName r2) of
        LT => LT
        GT => GT
        EQ => case compare (startLine r1) (startLine r2) of
                LT => LT
                GT => GT
                EQ => case compare (startColumn r1) (startColumn r2) of
                        LT => LT
                        GT => GT
                        EQ => case compare (endLine r1) (endLine r2) of
                                LT => LT
                                GT => GT
                                EQ => case compare (endColumn r1) (endColumn r2) of
                                        LT => LT
                                        GT => GT
                                        EQ => compare (metadata r1) (metadata r2)
