
# FUN - a functional programming language in Haskell 
### An implementation of the functional programming language F in Haskell
<br/>

## How to get started
1. The fun way
   ```Shell
   ghci Main.hs
   letsGo
   ```
2. The other way
   ```Shell
   ghci Main.hs
   emulate "your Fun code"
   ```
3. The hard way (*see at the bottom of the ReadMe)
   
<br/>

## Syntax changes
We made four adjustments to the F syntax:
1. We decided to interchange the semicolons between multiple `Local Definitions` with commas because we wanted semicolons to be used exclusively between multiple `Definitions` (for better readability).

2. According to the script the precedence of the negation should be lower than the precedence of an addition. This would lead to `-5 + 2` being evaluated as `-7`. We wanted our syntax to be more intuitive so we changed the precedences to let `-5 + 2` be evaluated as `-3`.
   
3. As opposed to the script we allow inputs of the form `A-B-C` and `A/B/C` by implementing a left associativity for `-` and `/`.
   
4.  We also implemented exponential expressions. Only integers are allowed as exponents.
<br/>

## Modules
### Main
The Main module is the interface for our entire implementation. It imports all of the modules below. You can either call the modules' functions directly or call **`letsGo`** for a fun and interactive experience of Fun!

Each module provides a `function` and a `showFunction`. When other modules want to continue processing the result of a different module they always call `function`, never `showFunction`. The only purpose of `showFunction` is to create a nice, more comprehensive output of an intermediate result.

### Tokenizer

- ### `tokenize`
    takes a `String` (your Fun program) as input, splits it up into `Tokens` and returns them in a list.
    
- ### `showTokenize`
    

### Parser
- ### `parse`
    takes a `String` (your Fun program) as input, calls `tokenize` on it and converts the list of `Tokens` into an AST (abstract syntax tree) with `Expressions` as nodes and leaves.

- ### `showParse`
    creates an output that is easier to comprehend than the output of `parse`. It lists all `Definitions` with their arguments and represents the AST structure of the function body with parentheses.
    
    For example `showParse "main = cool 1; cool x = 10*x;"`
    creates the following output:
    ```Haskell
    Definition main      []        (Function (Variable "cool") (Val 1))
    Definition cool      ["x"]     (Mult (Val 10) (Variable "x"))
    ```
### Compiler
- ### `compile`
    takes a `String` (your Fun program) as input, calls `parse` on it and converts the AST into a list of `Instructions`. This list is appended to `initCode` which contains the `Instructions` that call the `main` function, ensure the termination of the code and enable the emulation of unary, binary and if `Expressions`. `compile` also initializes `Heap`, `Global`, `PC` and `Stack`:
    ```Haskell
    pc     = 0
    code   = initCode ++ [Instruction]
    stack  = []
    heap   = [DEF name arity codeAddress]
    global = [(name, heapAddress)]
    ```
    `Heap` is initialized with `DEF` cells which consist of the name, arity and code address of each function. The `Global` environment contains the function names and corresponding `Heap` addresses of those `DEF` cells.
    
    These five sets of data are returned as a `State`:
    ``` Haskell
    State pc code stack heap global
    ```

- ### `showCompile`
    provides a better visualization of the initial `State`. It only shows `Instructions`, `Heap` and `Global`.

### Emulator
- ### `emulate`
    takes a `String` (your Fun program) as input, calls `compile` on it and uses the `State` as input for `run` which emulates the `Instructions` step by step and changes `PC`, `Stack` and `Heap` accordingly. It is a recursive function which overwrites the old `State` after each `Instruction` is processed. The final `State` contains the result of the emulation and is returned to `emulate` which unpacks the result and returns it.

- ### `showEmulate`
    visualizes `emulate` by showing each state of the emulation process. In order to do that, it needs to know how the `State` looked after each `Instruction` but run doesn't provide this information. So it calls `showRun` which doesn't overwrite the old `State`, but rather appends each new `State` to a list in a recursive manner.
   
<br/>

## Highlights
- letsGo function
- monadic error handling
- console output of `showFunctions`
- associativity of `-` and `/`
- exponential functions
- it works
- it's Fun!
  
<br/>

---

*3.   The hard way
   ```Haskell
   -- myProgram.hs
   import Tokenizer
   import Parser
   import Compiler
   import Emulator
   import Datatypes
   example = showTokenize "your Fun code"
   ```
   ```Shell
   ghci myProgram.hs
   example
   ```
