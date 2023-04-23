signature My_TOKENS =
sig
type ('a,'b) token
type svalue
val FFF:  'a * 'a -> (svalue,'a) token
val TTT:  'a * 'a -> (svalue,'a) token
val NEQEX:  'a * 'a -> (svalue,'a) token
val EQEXP:  'a * 'a -> (svalue,'a) token
val EQB:  'a * 'a -> (svalue,'a) token
val EXP:  'a * 'a -> (svalue,'a) token
val RATIONALNUM: (string) *  'a * 'a -> (svalue,'a) token
val POSNUMERAL: (string) *  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val GTE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LTE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val TODECIMAL:  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWRAT:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val MAKERAT:  'a * 'a -> (svalue,'a) token
val DIVRAT:  'a * 'a -> (svalue,'a) token
val MULRAT:  'a * 'a -> (svalue,'a) token
val SUBRAT:  'a * 'a -> (svalue,'a) token
val ADDRAT:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val FALSE: (string) *  'a * 'a -> (svalue,'a) token
val TRUE: (string) *  'a * 'a -> (svalue,'a) token
val BOOLEAN:  'a * 'a -> (svalue,'a) token
val INTEGER:  'a * 'a -> (svalue,'a) token
val RATIONAL:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature My_LRVALS=
sig
structure Tokens : My_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
