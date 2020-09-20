node(simferopol).
node(kharkiv).
node(tallin).
node(kaunas).
node(daugavpils).
node(odesa).
node(kyiv).
node(riga).
node(vilnius).
node(samara).
node(kaliningrad).
node(zhytomyr).
node(kishinev).
node(niznovgorod).
node(spb).
node(brest).
node(viciebsk).
node(volgograd).
node(donetsk).
node(ufa).
node(moscow).
node(murmansk).
node(minsk).
node(kazan).
node(oryol).
node(voronezh).
node(yaroslavl).


rout(vilnius, brest).
rout(viciebsk, brest).
rout(viciebsk, vilnius).
rout(voronezh, viciebsk).
rout(voronezh, volgograd).
rout(volgograd, viciebsk).
rout(viciebsk, niznovgorod).
rout(vilnius, daugavpils).
rout(kaliningrad, brest).
rout(kaliningrad, vilnius).
rout(kaunas, vilnius).
rout(kyiv, vilnius).
rout(kyiv, zhytomyr).
rout(zhytomyr, donetsk).
rout(zhytomyr, volgograd).
rout(kishinev, kyiv).
rout(kishinev, donetsk).
rout(spb, viciebsk).
rout(spb, kaliningrad).
rout(spb, riga).
rout(moscow, kazan).
rout(moscow, niznovgorod).
rout(moscow, minsk).
rout(moscow, donetsk).
rout(moscow, spb).
rout(murmansk, spb).
rout(murmansk, minsk).
rout(oryol, viciebsk).
rout(oryol, donetsk).
rout(oryol, moscow).
rout(odesa, kyiv).
rout(riga, kaunas).
rout(tallin, riga).
rout(kharkiv, kyiv).
rout(kharkiv, simferopol).
rout(yaroslavl, voronezh).
rout(yaroslavl, minsk).
rout(ufa, kazan).
rout(ufa, samara).

target(donetsk).

node(a). node(b). node(c).
node(d). node(e). node(f). node(g).

target(f).

rout(a,b).
rout(a,c).
rout(b,d).
rout(b,e).
rout(c,f).
rout(c,g).

move(A, B) :- rout(A,B);rout(B,A).

member(Elem, [Elem|_Tail]).
member(Elem, [_Head|Tail]) :- member(Elem, Tail).

dfs(From, To, _, [move(From, To)]) :-
	move(From, To),
	write(From -> ""),
	write(To).
dfs(From, To, VisitedNodes, [(From, X)|TailPath]) :-
	write(From -> ""),
	move(From, X),
	not(member(X,VisitedNodes)),
	dfs(X, To, [From|VisitedNodes], TailPath).

ddfs(From, To, _, _, [move(From, To)]) :-
	move(From, To),
	write(From -> ""),
	write(To).
ddfs(From, To, Counter, VisitedNodes, [(From, X)|TailPath]) :-
	write(From -> ""),
	C1 is Counter - 1,
	C1 > 1,
	move(From, X),
	not(member(X,VisitedNodes)),
	ddfs(X, To, C1, [From|VisitedNodes], TailPath).

width(From,_,Path) :-
	write(From -> ""),
	target(From).
width(From,Queue,[(From, To)|Tail]) :-
	move(From, To),
	not(member(To,Queue)),
	append(Queue,[To],Queue1),
	width(From,Queue1,Tail).
width(_,[Head|Tail],Path) :-
	width(Head,Tail,Path).