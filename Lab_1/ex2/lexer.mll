(**lexer.mll**)
(**Mihai-MCW - CSA M1 2018 UPS Tls3**)
{
(**Utility functions**)
let digit_of_char c = (int_of_char c - int_of_char '0')
let str_of_charNr n = (string_of_int (digit_of_char n))
class stack_of_ints =
     object (self)
   	val mutable the_list = ( [] : int list )
	method push x = the_list <- x :: the_list
	method merge x = the_list <- the_list @ [x]
	method elem x = List.nth the_list x
	method size = List.length the_list
end
let signs = new stack_of_ints
   and relat = new stack_of_ints
   and occ = new stack_of_ints
   and orig = new stack_of_ints
   and base = ref 0 and base_sign = ref 1
   and space = ref " " (*space delimiter*)
let print_input signs relat occ space = begin print_string("\nYour input is:\n"^string_of_int !base ^ "$");
	for i = 0 to (signs#size-1) do 
		for j = 1 to (occ#elem i) do
			if (signs#elem i) = 1 then print_string("+")
			else if (signs#elem i) = -1 then print_string("-")
			else print_string("0")
		done;
		if (relat#elem i) <> 0 then print_string(string_of_int(relat#elem i)^space)
		else print_string(space);
	done;
end
let calculate_S_from_CS signs relat occ = begin 
	for i = 0 to (signs#size-1) do
		for j = 0 to ((occ#elem i)-1) do
			orig#merge (!base + (signs#elem i) * (relat#elem i));
		done;
	done;
end
let print_output orig space = begin print_string("\nThe output is:\n");
	for i = 0 to (orig#size-1) do 
		print_string(string_of_int(orig#elem i)^" ") 
	done;
	print_string("\n");
end
let end_call_script signs relat occ orig spacing = begin 
	print_input signs relat occ spacing;
	calculate_S_from_CS signs relat occ; 
	print_output orig spacing;
end
}

(*** ANALYSER PART ***)
(** Lexical Unit(s) **)
let blank = ' ' | 'b'
let elf = eof | '\n'

(**Rules**)
rule
l0 = parse
	|['0'-'9'] as i	{ base := (digit_of_char i); l1 lexbuf }
	|'-'		{ base_sign := -1 ;l1 lexbuf }
	|_ 		{ failwith("error_l0: expected: int number or sign '-'\n") }
and l1 = parse
	|['0'-'9'] as i	{ base := !base * 10 + (digit_of_char i); l1 lexbuf }
	|'$'		{ base := !base * !base_sign; l2 lexbuf }
	|_ 		{ failwith("error_l1: expected: int number or $\n") }
and l2 = parse
	|'+'	{ signs#merge 1; let times = 1 in l3 times lexbuf }
	|'-'	{ signs#merge (-1); let times = 1 in l4 times lexbuf }
	|'0'	{ signs#merge 0; let times = 1 in l5 times lexbuf }
	|elf	{ if signs#size > 0 then (end_call_script signs relat occ orig !space) else failwith("error_l2: expected: +,- or 0\n") }
	|_ 	{ failwith("error_l2: expected: +,- or 0\n") }
and l3 times = parse
	|'+'		{ l3 (times+1) lexbuf }
	|['0'-'9'] as i	{ occ#merge times; let number = (digit_of_char i) in l6 number lexbuf }
	|_		{ failwith("error_l3: expected: int number or +\n") }
and l4 times = parse
	|'-'		{ l4 (times+1) lexbuf }
	|['0'-'9'] as i	{ occ#merge times; let number = (digit_of_char i) in l6 number lexbuf }
	|_		{ failwith("error_l3: expected: int number or -\n") }
and l5 times = parse
	|'0'		{ l5 (times+1) lexbuf }
	|blank		{ occ#merge times; relat#merge 0; l2 lexbuf }
	|elf		{ occ#merge times; relat#merge 0; end_call_script signs relat occ orig !space; }
	|_		{ failwith("error_l3: expected: 0 or blank space / 'b'\n") }
and l6 number = parse
	|['0'-'9'] as i	{ let number = number * 10 + (digit_of_char i) in l6 number lexbuf }
	|blank		{ relat#merge number; l2 lexbuf }
	|elf		{ relat#merge number; end_call_script signs relat occ orig !space; }
	|_		{ failwith("error_l6: expected: int number, blank space / 'b' or eol/eof\n") }
