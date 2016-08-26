# Contributor Information #

## Code Style Guidelines ##

### Do as an Expression  ###

`do`-expressions are expressions, so please don't stick the `do` alone by itself to hang out.

That is, write this:

```
doThing : List elem -> List (Nat, elem)
doThing xs =
    do x <- xs
       pure (42, x)
```

instead of this:
```
doThing : List elem -> List (Nat, elem)
doThing xs = do
    x <- xs
    pure (42, x)
```

### Trailing Infix Operators ###

When breaking a line at an infix operator, please put the operator before the newline.

Write this:
```
printInstructions : IO ()
printInstructions =
    putStrLn "Do one thing" *>
    putStrLn "And then another"
```

instead of this:
```
printInstructions : IO ()
printInstructions =
       putStrLn "Do one thing"
    *> putStrLn "And then another"
```


### Indentation ###

When indenting a complete definition, use four spaces. Example:
```
embiggen : Int -> Int
embiggen x =
    x + x + x + x + x
```

When indenting a `case` expression, use two spaces:
```
head : List elem -> Maybe elem
head xs =
    case xs of
      []      => Nothing
      x :: xs => Nothing
```

When using a failure pattern inside a `do` expression, use a new line and two spaces. Example:
```
do [] <- theList
     | x :: xs => pure ()
   pure ()
```

### Section Headings ###

To divide related "sections" of code, use headers like this:
```
----------------
-- I'm a Header!
----------------
```
with American English title capitalization conventions.


### Miscellany ###

Use explicit `implementation` keywords, even though they're optional.
