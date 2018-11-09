%token A
%token EOF
%type <unit> sp
%start sp

%%

sp: s EOF	{ () }
;

s:	u	        { print_string "S -> U\n" }
;

v:			{ print_string "U -> empty\n" }
;
w:			{ print_string "V -> empty\n" }
;
u: v w u		{ print_string "U -> V W U\n" }
| 	A		{ print_string "U -> a\n" }
;
%%
