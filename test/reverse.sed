# collect whole file in the pattern space
{
:loop
	N
	$!b loop
}

# iterativelly print last line and remove it
{
	h # to hold
:loop2
	g # get from hold
	s/^.*\n//
	p
	g
	s/\n[^\n]*$//
	h
	t loop2
}

d
