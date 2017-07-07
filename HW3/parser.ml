type token =
  | INT of (int)
  | BOOL of (bool)
  | VAR of (string)
  | CONSTRUCTOR of (string)
  | FUNCTION
  | PIPE
  | WILDCARD
  | FN_ARROW
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | EQ
  | IN
  | MATCH
  | WITH
  | LBRACK
  | RBRACK
  | SEMICOLON
  | LPAREN
  | RPAREN
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | GT
  | COLON
  | EOD
  | CONS

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
# 41 "parser.ml"
let yytransl_const = [|
  261 (* FUNCTION *);
  262 (* PIPE *);
  263 (* WILDCARD *);
  264 (* FN_ARROW *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* LET *);
  269 (* REC *);
  270 (* EQ *);
  271 (* IN *);
  272 (* MATCH *);
  273 (* WITH *);
  274 (* LBRACK *);
  275 (* RBRACK *);
  276 (* SEMICOLON *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* COMMA *);
  280 (* PLUS *);
  281 (* MINUS *);
  282 (* TIMES *);
  283 (* GT *);
  284 (* COLON *);
  285 (* EOD *);
  286 (* CONS *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* VAR *);
  260 (* CONSTRUCTOR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\007\000\007\000\008\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\013\000\013\000\013\000\015\000\
\015\000\014\000\014\000\014\000\006\000\006\000\016\000\016\000\
\004\000\004\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\018\000\018\000\018\000\019\000\019\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\006\000\001\000\004\000\004\000\006\000\
\001\000\003\000\003\000\001\000\003\000\001\000\003\000\003\000\
\001\000\003\000\001\000\002\000\001\000\002\000\001\000\001\000\
\003\000\002\000\003\000\003\000\001\000\001\000\001\000\003\000\
\003\000\000\000\001\000\003\000\003\000\004\000\004\000\005\000\
\001\000\003\000\001\000\002\000\002\000\001\000\001\000\003\000\
\002\000\003\000\003\000\000\000\001\000\003\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\029\000\030\000\024\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\057\000\000\000\002\000\
\000\000\009\000\000\000\000\000\017\000\000\000\021\000\023\000\
\047\000\000\000\046\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\045\000\000\000\000\000\049\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\028\000\000\000\027\000\000\000\000\000\000\000\
\000\000\013\000\018\000\000\000\048\000\051\000\000\000\050\000\
\006\000\042\000\000\000\003\000\000\000\000\000\007\000\036\000\
\000\000\033\000\054\000\000\000\056\000\000\000\000\000\000\000\
\008\000\004\000\000\000\000\000\038\000\000\000\000\000\000\000\
\040\000"

let yydgoto = "\002\000\
\014\000\015\000\038\000\053\000\017\000\087\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\039\000\042\000\101\000\
\033\000\054\000\057\000"

let yysindex = "\012\000\
\193\255\000\000\000\000\000\000\000\000\000\000\253\255\228\255\
\007\255\228\255\228\255\218\255\093\000\000\000\245\254\000\000\
\243\254\000\000\247\254\255\254\000\000\093\000\000\000\000\000\
\000\000\253\255\000\000\253\255\001\255\035\255\022\255\000\000\
\016\255\043\255\045\255\059\255\049\255\051\255\053\255\000\000\
\027\255\055\255\093\000\000\000\006\000\006\000\006\000\006\000\
\006\000\006\000\000\000\000\000\058\255\056\255\000\000\029\255\
\057\255\000\000\228\255\253\255\228\255\228\255\253\255\253\255\
\228\255\000\000\000\000\228\255\000\000\030\255\030\255\255\254\
\255\254\000\000\000\000\253\255\000\000\000\000\253\255\000\000\
\000\000\000\000\069\255\000\000\071\255\075\255\000\000\000\000\
\063\255\000\000\000\000\073\255\000\000\228\255\228\255\228\255\
\000\000\000\000\086\255\253\255\000\000\089\255\228\255\086\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\255\000\000\000\000\000\000\000\000\000\000\
\070\000\000\000\015\000\119\255\000\000\018\255\000\000\000\000\
\000\000\068\255\000\000\081\255\000\000\000\000\000\000\000\000\
\118\000\000\000\000\000\000\000\000\000\082\255\000\000\000\000\
\000\000\000\000\097\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\085\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\255\000\000\000\000\000\000\000\000\035\000\055\000\141\255\
\163\255\000\000\000\000\081\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\084\255\000\000\000\000\087\255\000\000\000\000\000\000\000\000\
\000\000\000\000\090\000\000\000\000\000\000\000\000\000\105\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\010\000\000\000\000\000\063\000\011\000\
\016\000\065\000\100\000\240\255\005\000\053\000\060\000\027\000\
\106\000\058\000\056\000"

let yytablesize = 397
let yytable = "\016\000\
\045\000\003\000\004\000\025\000\026\000\051\000\034\000\027\000\
\037\000\035\000\041\000\032\000\001\000\046\000\047\000\048\000\
\031\000\044\000\028\000\036\000\049\000\029\000\055\000\019\000\
\050\000\030\000\051\000\019\000\019\000\059\000\032\000\019\000\
\032\000\032\000\019\000\058\000\019\000\019\000\056\000\019\000\
\019\000\019\000\019\000\019\000\019\000\060\000\019\000\019\000\
\067\000\068\000\078\000\079\000\061\000\047\000\048\000\070\000\
\071\000\081\000\062\000\083\000\084\000\063\000\072\000\073\000\
\032\000\064\000\089\000\032\000\032\000\082\000\065\000\066\000\
\085\000\086\000\077\000\031\000\069\000\076\000\080\000\094\000\
\032\000\031\000\096\000\032\000\095\000\068\000\031\000\031\000\
\092\000\031\000\031\000\100\000\097\000\098\000\099\000\079\000\
\103\000\031\000\034\000\052\000\035\000\104\000\020\000\053\000\
\032\000\032\000\020\000\020\000\055\000\102\000\020\000\074\000\
\043\000\020\000\075\000\020\000\020\000\088\000\020\000\020\000\
\020\000\020\000\020\000\020\000\014\000\020\000\020\000\090\000\
\014\000\014\000\105\000\052\000\014\000\091\000\093\000\014\000\
\000\000\014\000\014\000\000\000\014\000\014\000\014\000\014\000\
\000\000\014\000\015\000\014\000\014\000\000\000\015\000\015\000\
\000\000\000\000\015\000\000\000\000\000\015\000\000\000\015\000\
\015\000\000\000\015\000\015\000\015\000\015\000\000\000\015\000\
\016\000\015\000\015\000\000\000\016\000\016\000\000\000\000\000\
\016\000\000\000\000\000\016\000\000\000\016\000\016\000\000\000\
\016\000\016\000\016\000\016\000\000\000\016\000\000\000\016\000\
\016\000\003\000\004\000\005\000\006\000\007\000\000\000\000\000\
\000\000\008\000\000\000\000\000\009\000\000\000\000\000\000\000\
\010\000\000\000\011\000\000\000\000\000\012\000\000\000\000\000\
\000\000\013\000\003\000\004\000\005\000\006\000\007\000\000\000\
\000\000\000\000\008\000\000\000\003\000\004\000\005\000\006\000\
\007\000\010\000\000\000\011\000\008\000\000\000\012\000\040\000\
\000\000\000\000\013\000\010\000\000\000\011\000\000\000\000\000\
\012\000\000\000\000\000\000\000\013\000\003\000\004\000\025\000\
\026\000\000\000\000\000\027\000\000\000\000\000\003\000\004\000\
\005\000\006\000\000\000\000\000\000\000\000\000\028\000\000\000\
\000\000\029\000\000\000\000\000\012\000\030\000\000\000\011\000\
\012\000\012\000\012\000\000\000\012\000\000\000\013\000\012\000\
\000\000\012\000\012\000\000\000\012\000\012\000\000\000\000\000\
\010\000\012\000\000\000\012\000\010\000\010\000\000\000\000\000\
\010\000\000\000\000\000\010\000\000\000\010\000\010\000\000\000\
\010\000\010\000\000\000\000\000\011\000\010\000\000\000\010\000\
\011\000\011\000\000\000\000\000\011\000\000\000\000\000\011\000\
\000\000\011\000\011\000\005\000\011\000\011\000\000\000\005\000\
\005\000\011\000\000\000\011\000\000\000\000\000\005\000\000\000\
\005\000\005\000\000\000\005\000\005\000\003\000\004\000\005\000\
\006\000\000\000\005\000\037\000\037\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\037\000\037\000\011\000\037\000\
\037\000\012\000\039\000\039\000\000\000\000\000\037\000\000\000\
\000\000\039\000\000\000\039\000\039\000\041\000\039\000\039\000\
\000\000\000\000\000\000\041\000\000\000\039\000\000\000\000\000\
\041\000\041\000\000\000\041\000\041\000"

let yycheck = "\001\000\
\014\001\001\001\002\001\003\001\004\001\022\000\008\000\007\001\
\010\000\003\001\012\000\007\000\001\000\027\001\024\001\025\001\
\007\000\029\001\018\001\013\001\030\001\021\001\022\001\006\001\
\026\001\025\001\043\000\010\001\011\001\008\001\026\000\014\001\
\028\000\029\000\017\001\001\001\019\001\020\001\029\000\022\001\
\023\001\024\001\025\001\026\001\027\001\030\001\029\001\030\001\
\022\001\023\001\022\001\023\001\010\001\024\001\025\001\045\000\
\046\000\059\000\014\001\061\000\062\000\003\001\047\000\048\000\
\060\000\017\001\068\000\063\000\064\000\060\000\020\001\019\001\
\063\000\064\000\019\001\008\001\022\001\020\001\022\001\011\001\
\076\000\014\001\008\001\079\000\014\001\023\001\019\001\020\001\
\079\000\022\001\023\001\006\001\094\000\095\000\096\000\023\001\
\008\001\030\001\019\001\019\001\019\001\103\000\006\001\019\001\
\100\000\022\001\010\001\011\001\022\001\100\000\014\001\049\000\
\013\000\017\001\050\000\019\001\020\001\065\000\022\001\023\001\
\024\001\025\001\026\001\027\001\006\001\029\001\030\001\068\000\
\010\001\011\001\104\000\026\000\014\001\076\000\079\000\017\001\
\255\255\019\001\020\001\255\255\022\001\023\001\024\001\025\001\
\255\255\027\001\006\001\029\001\030\001\255\255\010\001\011\001\
\255\255\255\255\014\001\255\255\255\255\017\001\255\255\019\001\
\020\001\255\255\022\001\023\001\024\001\025\001\255\255\027\001\
\006\001\029\001\030\001\255\255\010\001\011\001\255\255\255\255\
\014\001\255\255\255\255\017\001\255\255\019\001\020\001\255\255\
\022\001\023\001\024\001\025\001\255\255\027\001\255\255\029\001\
\030\001\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\009\001\255\255\255\255\012\001\255\255\255\255\255\255\
\016\001\255\255\018\001\255\255\255\255\021\001\255\255\255\255\
\255\255\025\001\001\001\002\001\003\001\004\001\005\001\255\255\
\255\255\255\255\009\001\255\255\001\001\002\001\003\001\004\001\
\005\001\016\001\255\255\018\001\009\001\255\255\021\001\022\001\
\255\255\255\255\025\001\016\001\255\255\018\001\255\255\255\255\
\021\001\255\255\255\255\255\255\025\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\255\255\255\255\001\001\002\001\
\003\001\004\001\255\255\255\255\255\255\255\255\018\001\255\255\
\255\255\021\001\255\255\255\255\006\001\025\001\255\255\018\001\
\010\001\011\001\021\001\255\255\014\001\255\255\025\001\017\001\
\255\255\019\001\020\001\255\255\022\001\023\001\255\255\255\255\
\006\001\027\001\255\255\029\001\010\001\011\001\255\255\255\255\
\014\001\255\255\255\255\017\001\255\255\019\001\020\001\255\255\
\022\001\023\001\255\255\255\255\006\001\027\001\255\255\029\001\
\010\001\011\001\255\255\255\255\014\001\255\255\255\255\017\001\
\255\255\019\001\020\001\006\001\022\001\023\001\255\255\010\001\
\011\001\027\001\255\255\029\001\255\255\255\255\017\001\255\255\
\019\001\020\001\255\255\022\001\023\001\001\001\002\001\003\001\
\004\001\255\255\029\001\010\001\011\001\255\255\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\020\001\018\001\022\001\
\023\001\021\001\010\001\011\001\255\255\255\255\029\001\255\255\
\255\255\017\001\255\255\019\001\020\001\008\001\022\001\023\001\
\255\255\255\255\255\255\014\001\255\255\029\001\255\255\255\255\
\019\001\020\001\255\255\022\001\023\001"

let yynames_const = "\
  FUNCTION\000\
  PIPE\000\
  WILDCARD\000\
  FN_ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  MATCH\000\
  WITH\000\
  LBRACK\000\
  RBRACK\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  GT\000\
  COLON\000\
  EOD\000\
  CONS\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  VAR\000\
  CONSTRUCTOR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 36 "parser.mly"
           ( print ";;"; _1 )
# 304 "parser.ml"
               : modecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 39 "parser.mly"
        ( print "d -> e"; Expr(_1) )
# 311 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 40 "parser.mly"
                   ( print "d -> let x = e"; Let(_2,_4) )
# 319 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 42 "parser.mly"
      ( print "d -> let rec f p = e"; LetRec(_3,Function(_4,_6)) )
# 328 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compareexp) in
    Obj.repr(
# 46 "parser.mly"
               ( _1 )
# 335 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 47 "parser.mly"
                                  ( print "e -> function p -> e"; Function(_2,_4) )
# 343 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patterns) in
    Obj.repr(
# 48 "parser.mly"
                               ( print "e -> match e with ps"; Match(_2,_4) )
# 351 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 49 "parser.mly"
                             ( print "e -> if e then e else e"; If(_2,_4,_6) )
# 360 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 53 "parser.mly"
           ( _1 )
# 367 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 54 "parser.mly"
                          ( BinOp(_1,Eq,_3) )
# 375 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 55 "parser.mly"
                          ( BinOp(_1,Gt,_3) )
# 383 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 59 "parser.mly"
            (_1 )
# 390 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 60 "parser.mly"
                         ( print "e -> e::e"; Data("Cons", Some (Tuple [_1 ; _3])) )
# 398 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 64 "parser.mly"
             ( _1 )
# 405 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 65 "parser.mly"
                          ( BinOp(_1,Plus,_3) )
# 413 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 66 "parser.mly"
                           ( BinOp(_1,Minus,_3) )
# 421 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 70 "parser.mly"
           ( _1 )
# 428 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'timesexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 71 "parser.mly"
                          ( BinOp(_1,Times,_3) )
# 436 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 75 "parser.mly"
           ( _1 )
# 443 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 76 "parser.mly"
                 ( Negate(_2) )
# 450 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 80 "parser.mly"
            ( print "ae -> be"; _1 )
# 457 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 81 "parser.mly"
                     (
               match _1 with
                 Data(c,None) -> Data(c,Some _2)
                | _ -> FunctionCall(_1,_2)
             )
# 469 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 88 "parser.mly"
        ( print "be -> c"; _1 )
# 476 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
          ( print ("be -> var "^_1); Var(_1) )
# 483 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'explist) in
    Obj.repr(
# 90 "parser.mly"
                          ( _2 )
# 490 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                  ( Tuple [] )
# 496 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exptuple) in
    Obj.repr(
# 92 "parser.mly"
                           ( Tuple _2 )
# 503 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 93 "parser.mly"
                      ( print "be -> (e)"; _2 )
# 510 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "parser.mly"
         ( print ("c -> int "^(string_of_int _1)); IntConst(_1) )
# 517 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 98 "parser.mly"
         ( print ("c -> bool "^(string_of_bool _1)); BoolConst(_1) )
# 524 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                  ( Data(_1, None) )
# 531 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 103 "parser.mly"
                ( [_1; _3] )
# 539 "parser.ml"
               : 'exptuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exptuple) in
    Obj.repr(
# 104 "parser.mly"
                     ( _1 :: _3 )
# 547 "parser.ml"
               : 'exptuple))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
  ( Data("Nil", None) )
# 553 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 109 "parser.mly"
      ( Data("Cons", Some (Tuple [_1 ; Data("Nil", None)])) )
# 560 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'explist) in
    Obj.repr(
# 110 "parser.mly"
                        ( Data("Cons", Some (Tuple [_1 ; _3])) )
# 568 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 114 "parser.mly"
                         ( print "fp -> p -> e"; [(_1,_3)] )
# 576 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 115 "parser.mly"
                                        ( print "fp -> e fpA"; (_1,_3) :: _4 )
# 585 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 119 "parser.mly"
                              ( print "fpA -> | p -> e"; [(_2,_4)] )
# 593 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 120 "parser.mly"
                                             ( print "fpA -> | p -> e fpA"; (_2,_4) :: _5 )
# 602 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basepattern) in
    Obj.repr(
# 124 "parser.mly"
                ( _1 )
# 609 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basepattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 125 "parser.mly"
                               ( DataPat("Cons", Some (TuplePat [_1; _3])) )
# 617 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 129 "parser.mly"
          ( print "p -> c";
				match _1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Data(c, None) -> DataPat(c, None)
				  | _ -> raise ConstTypeParseError )
# 629 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 135 "parser.mly"
                 ( print ("p -> -int "^(string_of_int (_2 * -1))); IntPat(_2 * -1) )
# 636 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'basepattern) in
    Obj.repr(
# 136 "parser.mly"
                              ( DataPat(_1, Some _2) )
# 644 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
               ( print "p -> _"; WildcardPat )
# 650 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
          ( print ("p -> var "^_1^":t"); VarPat(_1) )
# 657 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'patlist) in
    Obj.repr(
# 139 "parser.mly"
                            ( _2 )
# 664 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                    ( TuplePat [] )
# 670 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattuple) in
    Obj.repr(
# 141 "parser.mly"
                             ( TuplePat _2 )
# 677 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 142 "parser.mly"
                            ( print "p -> (p)"; _2 )
# 684 "parser.ml"
               : 'basepattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
   ( DataPat("Nil", None) )
# 690 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 147 "parser.mly"
          ( DataPat("Cons", Some (TuplePat [_1 ; DataPat("Nil", None)])) )
# 697 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'patlist) in
    Obj.repr(
# 148 "parser.mly"
                            ( DataPat("Cons", Some (TuplePat [_1 ; _3])) )
# 705 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 152 "parser.mly"
                        ( [_1; _3] )
# 713 "parser.ml"
               : 'pattuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattuple) in
    Obj.repr(
# 153 "parser.mly"
                         ( _1 :: _3 )
# 721 "parser.ml"
               : 'pattuple))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : modecl)
