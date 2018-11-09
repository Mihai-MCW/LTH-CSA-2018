{
	open Parser
}

rule main = parse 
	|	'a'		{ A }
	|	'b'		{ B }
	|	' '		{ L }
	|	'\n'	        { EOF }
	|	eof		{ EOF }
	|	_		{ failwith "bad character !" }
