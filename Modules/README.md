
# FUN! 
### An implementation of the functional programming language F in Haskell
<br/>

## Modules

- Tokenizer (tokenize, showTokenize)
   
    
- Parser (parse, showParse)

  sdbnviowbvi
- Compiler (compile, showCompile)
- Emulator (emulate, showEmulate)
- Datatypes
- Show
- Main

    The Main module is an interface for our entire implementation. It imports all of the modules above and allows you to call the following functions:

    Each module provides a `showFunction`, which creates a nicer output than the main `function` of the module. When other modules want to call `function`.
    ### `tokenize`
    takes a `String` (your Fun program) as input, splits it up into `Tokens` and returns them in a list.
    
    ### `showTokenize`
    creates a nicer console output for `tokenize`.

    ### `parse`
    takes a `String` (your Fun program) as input, calls `tokenize` on it and converts the list of `Tokens` into an AST (abstract syntax tree) with `Expressions` as nodes and leafs.

    ### `showParse`
    creates an output, that is easier to comprehend than the output of `parse`. It lists all `Definitions` with their arguments and represents the AST structure of the function body with parentheses.
    
    For example `showParse "main = cool 1; cool x = 10*x;"`
    creates the following output:
    ```Haskell
    Definition main      []        (Function (Variable "cool") (Val 1))
    Definition cool      ["x"]     (Mult (Val 10) (Variable "x"))
    ```

    ### `compile`
    takes a `String` (your Fun program) as input, calls `parse` on it and converts the AST into a list of `Instructions`. This is list is appended to `initCode`, which contains the `Instructions` that call the `main` funcion, ensure the termination of the code and enable the emulation of unary, binary and if `Expressions`. `compile` also initializes `Heap`, `Global`, `PC` and `Stack`:
    ```Haskell
    pc     = 0
    code   = initCode ++ [Instruction]
    stack  = []
    heap   = [DEF name arity codeAddress]
    global = [(name, heapAddress)]
    ```
    `Heap` is initialized with `DEF` cells, which consist of the name, arity and code address of each function. The `Global` environment contains the function names and corresponding `Heap` addresses of those `DEF` cells.
    
    These five sets of data are returned as a `State`:
    ``` Haskell
    State pc code stack heap global
    ```

    ### `showCompile`
    provides a better visualization of the initial `State`. It shows neither `PC` nor `Stack`.

    ### `emulate`
    takes a `String` (your Fun program) as input, calls `compile` on it and uses the `State` as input for `run`. The `Instructions` are emulated step by step and can change `PC`, `Stack` and `Heap`. `run` is a recursive function which overwrites the old `State` after each `Instruction` is processed. The final `State` contains the result of the emulation and is returned to `emulate`, which unpacks the result and returns it.

    ### `showEmulate`
    viualizes `emulate` by showing each state of the emulation process. In order to do that, it needs to know how the `State` looked afte each instruction, but run doesn't provide this information. So it calls `showRun` which doesn't overwrite the old State, but rather appends each new `State` to a list.


- Syntaxänderungen!!!!
- rein funktionelle Funktionen vs showTokens, showParse, showCompile, showEmulate
- Modularisierung (nur relevante Funktionen)
- Stack aus Strings nur für Ausgabe
- Anleitung: Main laden und entweder letsgooo! oder manuell Funktionen aufrufen oder Beispiele verwenden
- alles monadische Fehlerbehandlung bitches yeah!

## To-Do:
- README schreiben
- Kommentare entfernen und neue hinzufügen
- Planung für Präsentation außerhalb des Abgabe-Codes
- Syntax schöner
- Right, dann Left! überall!