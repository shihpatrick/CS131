%SIGNAL_MORSE 

count_lead1s([],0,[]).
count_lead1s([0|T],0,[0|T]).
count_lead1s([1|T],N,R):- count_lead1s(T,N1,R), N is N1+1.

count_lead0s([],0,[]).
count_lead0s([1|T],0,[1|T]).
count_lead0s([0|T], N, R):-count_lead0s(T,N1,R), N is N1+1.

convert1s(1,'.').
convert1s(2,'.').
convert1s(N, -) :- N >= 2.

convert0s(2, ^).
convert0s(3, ^).
convert0s(4, ^).
convert0s(5, ^).
convert0s(N, #) :- N >= 5. 


signal_morse([], []).
signal_morse([0],[]).
signal_morse([1|T],[Sym|MT]) :-
	count_lead1s([1|T],Num,Rem),
	convert1s(Num,Sym),
	signal_morse(Rem,MT).
 
signal_morse([0,1|T],M):-signal_morse([1|T], M). 
signal_morse([0,0,1|T],M):-signal_morse(T,M).

signal_morse([0|T],[Sym|MT]) :-
	count_lead0s([0|T],Num,Rem),
	convert0s(Num,Sym),
	signal_morse(Rem,MT).

morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)

get_head([],[],[]).
get_head([^|T],[],T).
get_head([X,#|T],[X],[#|T]).
get_head([Signal|T],[Signal|TS],Rem) :- 
	get_head(T,TS,Rem).

get_message([],[]).
get_message([#|T],[#|TH]):-
	get_message(T,TH).

get_message(Morse,[Char|T]) :-
	get_head(Morse,Head,Remain),
	morse(Char,Head),
	get_message(Remain,T).

reverse3([],A,A).
reverse3([H|T],A,Rev):-
	reverse3(T,[H|A],Rev).
reverse2(L,R):-reverse3(L,[],R).

consume([],[]).
consume([error|T],[error|T]):-!.
consume([#|T],[#|T]):-!.
consume([H|T],New):-
	consume(T,New).

clean_errors([],[]).
clean_errors([error,error|T],[error|T2]):-
	clean_errors([error|T],T2),!.
clean_errors([error|T], T2):-
	consume(T,New),
	clean_errors(New,T2).
clean_errors([H|T],[H|T2]):-
	clean_errors(T,T2).

signal_message(L,Message):-
	signal_morse(L,Morse),
	get_message(Morse,Full),
	reverse2(Full,Rev),
	clean_errors(Rev,RevM),
	reverse2(RevM, Message).
