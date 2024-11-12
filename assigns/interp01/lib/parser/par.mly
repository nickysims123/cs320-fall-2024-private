%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF
%token THEN
%token ELSE
%token LET
%token IN 
%token FUN 
%token ARR
%token UNIT 
%token LPAREN
%token RPAREN
%token TRUE 
%token FALSE 
%token PLUS 
%token MINUS 
%token TIMES
%token DIV
%token MOD 
%token LT 
%token LTE 
%token GT 
%token GTE 
%token EQ 
%token NEQ 
%token AND 
%token OR

%right OR 
%right AND

%left LT LTE GT GTE EQ NEQ
%left PLUS MINUS 
%left TIMES DIV MOD 

%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | IF; check = expr; THEN; e1 = expr; ELSE; e2 = expr { If (check, e1, e2) }
  | LET; name = VAR; EQ; e1 = expr; IN; e2 = expr { Let (name, e1, e2)}
  | FUN; name = VAR; ARR; e = expr { Fun (name, e)}
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 {Bop (op, e1, e2)}
  | e = expr3; es = expr3* { mk_app e es }

expr3: 
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e }
%inline bop: 
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ {Neq}
  | AND { And }
  | OR { Or } 
