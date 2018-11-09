%token A
%token B
%token L
%token EOF
%type <unit> sp
%start sp

%%

sp: s EOF	{ ( ) }
;

s: u v A 	{ print_string "S -> U V a\n" }
;

u:	A	        { print_string "U -> a\n" }
;

v:	w		{ print_string "V -> W\n" }
|	B		{ print_string "V -> b\n" }
;

w:	L		{ print_string "W -> empty\n" }
;
%%
