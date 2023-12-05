# COL226 ASSIGNMENT 4 :  THE IMPERATIVE LANGUAGE RATIONAL PL-0
In this assignment we implemented a RATIONAL PL0 WHILE language, i.e. 
we first generated a AST for a given input then traversed over that AST to 
update the variables in the global scope, we also implemented recursive calling,
all this was done using recursive descent TOP-DOWN parser implemented in SML.

# CONTEXT FREE GRAMMAR 
```
NonTerminals (  in `lowercase` ) : 

    start of AST | blk of BLK | 
    decseq of DEC list | comseq of CMD list | commands of CMD list |
    dec of DEC | varlist of string list |
    typedec of Type | pdeflist of PDEF list | procdef of PDEF |
    command of CMD | expression of Exp | typedec2 of Type2 |
    rationalex of REXP | integerex of IEXP |
    boolex of BEXP

Terminals ( in `UpperCase` ) :

    EOF | RATIONAL | INTEGER | BOOLEAN | TRUE of string |
    FALSE of string | VAR | IF | THEN | ELSE | FI | WHILE |
    PROCEDURE | PRINT | CALL | READ | NEG | INVERSE |
    ADDRAT | SUBRAT | MULRAT | DIVRAT | MAKERAT | RAT |
    SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL |
    ADD | SUB | MUL | DIV | MOD | NOT | AND | OR | EQ |
    NEQ | LT | LTE | GT | GTE | ASSIGN | SEMICOLON | COMMA |
    LPAREN | RPAREN | LBRACE | RBRACE | IDENTIFIER of string |
    POSNUMERAL of string | RATIONALNUM of string | EXP |
    EQB | EQEXP | NEQEX | TTT | FFF | DO | OD


```

Detailed Grammar in "my.yacc" file

# DATA TYPES 

```
datatype AST = PROG of BLK
and BLK = BLK of (DEC list) * (CMD list)
and DEC = VDEC of ( Type * (string list) ) | PDEC of PDEF list 
and PDEF = PDEF of Type2 * string * BLK
and Type = Int | Bool | Rational 
and Type2 = procedure
and CMD = SET of string * Exp | Call of string | Read of string | Print of Exp | ITE of Exp * ( CMD list ) * ( CMD list ) | WH of Exp * ( CMD list )
and Exp = REXP of REXP | IEXP of IEXP | BEXP of BEXP | EXP of Exp | EQEXP of Exp * Exp | NEQEX of Exp * Exp
and REXP = NEGR of REXP 
        | INVERSE of REXP
        | ADDRAT of REXP * REXP
        | SUBRAT of REXP * REXP
        | MULRAT of REXP * REXP
        | DIVRAT of REXP * REXP
        | MAKERAT of IEXP * IEXP
        | RAT of IEXP
        | SHOWRAT of REXP
        | SHOWDECIMAL of REXP
        | FROMDECIMAL of REXP
        | TODECIMAL of REXP
        | RNUM of string
        | IDR of string
        | EQR of REXP * REXP
        | NEQR of REXP * REXP
        | GTR of REXP * REXP
        | GTER of REXP * REXP
        | LTR of REXP * REXP
        | LTER of REXP * REXP
and IEXP = NEG of IEXP
        | ADD of IEXP * IEXP
        | SUB of IEXP * IEXP
        | MUL of IEXP * IEXP 
        | DIV of IEXP * IEXP
        | MOD of IEXP * IEXP
        | EQ  of IEXP * IEXP
        | NEQ of IEXP * IEXP
        | GT  of IEXP * IEXP
        | GTE of IEXP * IEXP
        | LT  of IEXP * IEXP
        | LTE of IEXP * IEXP
        | ID of string
        | POSNUM of string
and BEXP = NOT of BEXP
        | AND of BEXP * BEXP
        | OR of BEXP * BEXP
        | EQB of BEXP * BEXP
        | NEQB of BEXP * BEXP
        | TTT of string
        | FFF of string
        | IDB of string

```
This is the datatype implemented for the Abstrct Syntax Tree of the lexer and parser.
This is what we traverse in the interpreter.

# USAGE 

```
1. Open sudo sml
2. compile "my.cm"
3. then if you want the AST then run My.compile(filename)
4. else if you want the interpreter to read from filein and write the output in fileout then
5. run My.interpreter(filein,fileout)

SPECIAL NOTE : since the lists are global and mutable typed therefore for every use kindly recompile using the bogus function,
just change the value of the bogus and recompile that empties the list.


```


# DESIGN DECISIONS

```
1. The grammar in EBNF for RatExp | BoolExp | IntExp was not given so I extended the grammar to the following
    RatExp -> rational ( an expression as defined in the grammar above using binary and unary operators. )
    BoolExp -> boolean ( an expression as defined in the grammar above using binary and unary operators. )
    IntExp -> integer ( an expression as defined in the grammar above using binary and unary operators. )

2. The input from user side when using read is "true" and "false" instead of tt and ff , to use pre-existing functions
 as I convert tt and ff to true and false as it is for storing .

3. To prevent conflicts try to use (exp) instead of exp.

Other design decisions are self-explanatory: the AST datatype incorporates references and assignment variable types, to make semantic analysis easier for the next stages.
```

# ERROR HANDLING 

```
1. NotFoundTheElement : when you try to access any element that is not yet declared .
2. NotFoundTheElement : when you try to access any procedure that is not yet declared .
3. RatErrors raised in Rational.sml file.
```

# Acknowledgements

1. https://github.com/ChinmayMittal/COL226 ( Github Repository 1)
2. https://github.com/Aniruddha-Deb/COL226 ( Github Repository 2)
3. https://github.com/5ayam5/Boolean-Lexer-and-Parser ( Github Repository 3)
4. https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43247bea84122c52aa2d86aa86a8f4997825d419 ( Official Documentation )

