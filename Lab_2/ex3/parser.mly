%token A
%token B
%token C
%token D
%token EOF
%type <unit> sp
%start sp

%%

sp: s EOF	{ () }
;

s:	u A	        { print_string "S -> U a\n" }
|	B u C		{ print_string "S -> b U c\n" }
|	u C		{ print_string "S -> U c\n" }
|	B u A		{ print_string "S -> b U a\n" }
;
u:	D		{ print_string "U -> d\n" }
;
%%
