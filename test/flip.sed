{
	h
:loop
	g
	s/^.*(.)$/\1/
	p
	g
	s/.$//
	h
	/^$/! b loop
}

d
