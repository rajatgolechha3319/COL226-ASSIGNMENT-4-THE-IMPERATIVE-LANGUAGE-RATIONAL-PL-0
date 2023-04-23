open DataTypes;

%%
%name My

%term EOF | RATIONAL | INTEGER | BOOLEAN | TRUE of string | FALSE of string |
            VAR | IF | THEN | ELSE | FI | WHILE | DO | OD |
            PROCEDURE | PRINT | CALL | READ | NEG | INVERSE |
            ADDRAT | SUBRAT | MULRAT | DIVRAT | MAKERAT | RAT |
            SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL |
            ADD | SUB | MUL | DIV | MOD | NOT | AND | OR | EQ |
            NEQ | LT | LTE | GT | GTE | ASSIGN | SEMICOLON | COMMA |
            LPAREN | RPAREN | LBRACE | RBRACE | IDENTIFIER of string | POSNUMERAL of string | RATIONALNUM of string | EXP |
            EQB | EQEXP | NEQEX | TTT | FFF

%nonterm start of AST | blk of BLK | 
        decseq of DEC list | comseq of CMD list | commands of CMD list |
        dec of DEC | varlist of string list |
        typedec of Type | pdeflist of PDEF list | procdef of PDEF |
        command of CMD | expression of Exp | typedec2 of Type2 |
        rationalex of REXP | integerex of IEXP |
        boolex of BEXP

%eop EOF 
%noshift EOF
%pos int
%verbose

%right ASSIGN
%left OR AND
%left GT GTE LT LTE
%left ADD SUB
%left MUL MOD DIV
%left ADDRAT SUBRAT
%left MULRAT DIVRAT
%right NOT NEG
%left EQ NEQ
%left LPAREN RPAREN
%left LBRACE RBRACE

%%

start: blk ( PROG(blk))

blk: decseq comseq (BLK(decseq, comseq))

decseq: (([]))
        | dec decseq ( (dec::decseq))

dec: typedec varlist SEMICOLON (VDEC(typedec,varlist)) 
        | pdeflist (PDEC(pdeflist))

typedec:  INTEGER ((Int))
        | BOOLEAN ((Bool))
        | RATIONAL ((Rational))

typedec2: PROCEDURE ((procedure))

varlist:  IDENTIFIER (([IDENTIFIER]))
        | IDENTIFIER COMMA varlist  ((IDENTIFIER::varlist))

pdeflist: (([]))
        | procdef SEMICOLON  pdeflist ((procdef::pdeflist))

procdef: typedec2 IDENTIFIER blk (PDEF(typedec2,IDENTIFIER,blk))

comseq: LBRACE commands RBRACE ((commands))

commands: (([]))
        | command SEMICOLON commands ((command::commands))

command: IDENTIFIER ASSIGN expression (SET(IDENTIFIER,expression))
        | CALL IDENTIFIER (Call(IDENTIFIER))
        | READ LPAREN IDENTIFIER RPAREN (Read(IDENTIFIER))
        | PRINT LPAREN expression RPAREN (Print(expression))
        | IF expression THEN comseq ELSE comseq FI (ITE(expression,comseq1,comseq2))
        | WHILE expression DO comseq OD (WH(expression,comseq))

expression: RATIONAL rationalex (REXP(rationalex))
        | INTEGER integerex (IEXP(integerex))
        | BOOLEAN boolex (BEXP(boolex))
        | LPAREN expression RPAREN (EXP(expression))
        | expression EQ expression (EQEXP(expression1,expression2))
        | expression NEQ expression (NEQEX(expression1,expression2))

rationalex: NEG rationalex (NEGR(rationalex))
        | INVERSE rationalex (INVERSE(rationalex))
        | rationalex ADDRAT rationalex (ADDRAT(rationalex1,rationalex2))
        | rationalex SUBRAT rationalex (SUBRAT(rationalex1,rationalex2))
        | rationalex MULRAT rationalex (MULRAT(rationalex1,rationalex2))
        | rationalex DIVRAT rationalex (DIVRAT(rationalex1,rationalex2))
        | MAKERAT LPAREN integerex COMMA integerex RPAREN (MAKERAT(integerex1,integerex2))
        | RAT LPAREN integerex RPAREN (RAT(integerex))
        | SHOWRAT rationalex (SHOWRAT(rationalex))
        | SHOWDECIMAL LPAREN rationalex RPAREN (SHOWDECIMAL(rationalex))
        | FROMDECIMAL LPAREN rationalex RPAREN (FROMDECIMAL(rationalex))
        | TODECIMAL LPAREN rationalex RPAREN (TODECIMAL(rationalex))
        | RATIONALNUM ((RNUM(RATIONALNUM)))
        | IDENTIFIER ((IDR(IDENTIFIER)))
        | rationalex EQ rationalex  (EQR(rationalex1,rationalex2))
        | rationalex NEQ rationalex (NEQR(rationalex1,rationalex2))
        | rationalex LT rationalex  (LTR(rationalex1,rationalex2))
        | rationalex LTE rationalex (LTER(rationalex1,rationalex2))
        | rationalex GT rationalex  (GTR(rationalex1,rationalex2))
        | rationalex GTE rationalex (GTER(rationalex1,rationalex2))

integerex: NEG integerex (NEG(integerex))
        |  integerex ADD integerex (ADD(integerex1,integerex2))
        |  integerex SUB integerex (SUB(integerex1,integerex2))
        |  integerex MUL integerex (MUL(integerex1,integerex2))
        |  integerex DIV integerex (DIV(integerex1,integerex2))
        |  integerex MOD integerex (MOD(integerex1,integerex2))
        | integerex EQ integerex  (EQ(integerex1,integerex2))
        | integerex NEQ integerex (NEQ(integerex1,integerex2))
        | integerex LT integerex  (LT(integerex1,integerex2))
        | integerex LTE integerex (LTE(integerex1,integerex2))
        | integerex GT integerex  (GT(integerex1,integerex2))
        | integerex GTE integerex (GTE(integerex1,integerex2))
        | IDENTIFIER ((ID(IDENTIFIER)))
        | POSNUMERAL ((POSNUM(POSNUMERAL)))

boolex: NOT boolex (NOT(boolex))
        | boolex AND boolex (AND(boolex1,boolex2))
        | boolex EQ boolex (EQB(boolex1,boolex2))
        | boolex NEQ boolex (NEQB(boolex1,boolex2))
        | boolex OR boolex (OR(boolex1,boolex2))
        | TRUE (TTT(TRUE))
        | FALSE (FFF(FALSE))
        | IDENTIFIER ((IDB(IDENTIFIER)))





