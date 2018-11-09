%token A
%token B
%token C
%token EOF
%type <unit> sp
%start sp

%%

sp: s EOF	{ () }
;

s:	A u B	        { print_string "S -> a U b\n" }
;

u:	C		{ print_string "U -> c\n" }
|	u C		{ print_string "U -> U c\n" }
;

%%
