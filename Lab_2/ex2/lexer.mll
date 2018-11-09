{
	open Parser
}

rule main = parse 
	|	'a'		{ A }
	|	'b'		{ B }
	|	'c'		{ C }
	|	' '		{ L }
	|	'\n'	        { EOF }
	|	eof		{ EOF }
	|	_		{ failwith "bad character !" }
