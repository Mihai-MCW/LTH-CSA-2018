(** lexer.mll *)
{
(*** OCAML PART for tool functions ***)
(* convert char in int *)
let digit_of_char c = (int_of_char c - int_of_char '0')
}

(*** ANALYSER PART ***)
(** Lexical Unit(s) **)
let integer = ['0' - '9']

(** Rules **)
rule l0 = parse 
        | integer as i { (*A1*) let base = digit_of_char i in l1 base lexbuf }
	| _    {failwith "Invalid character in l0" } (** to exit properly, or add another rule *)
and
l1 base = parse
	| integer as i   { (*A2*) let base = base * 10 + digit_of_char i in l1 base lexbuf }
	| '\n'   	{ (*A3*) print_string ("The number in base 10 is "^string_of_int base^"\n") }
	| eof	        { (*A3*) print_string ("The number in base 10 is "^string_of_int base^"\n") }
	|'$'		{ (*A4*) if base < 2 || base > 10 then failwith "Base is not good" else l2 base lexbuf;}
	| _    {failwith "Invalid character in l1" }
and 
l2 base = parse
	| integer as i   { (*A5*) let digit = digit_of_char i in if digit >= base then failwith "Number is not in base" else l3 digit base lexbuf}
	| _    {failwith "erreur_l2" }
and 
l3 digit base = parse
	| integer as i   { (*A6*) let digit = digit + base + digit_of_char i in l3 digit base lexbuf}
	| '\n'		{ (*A7*) print_string ("The number in base "^string_of_int base^" is "^string_of_int digit^"\n")}
	| eof	        { (*A7*) print_string ("A7 reads eof"^"\n")}
	| _    {failwith "erreur_l3" }
