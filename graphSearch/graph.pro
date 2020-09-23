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

distance(simferopol,	716).
distance(kharkiv,		541).
distance(tallin,		1052).
distance(kaunas,		610).
distance(daugavpils,	641).
distance(odesa,			449).
distance(kyiv,			134).
distance(riga,			802).
distance(vilnius,		545).
distance(samara,		1510).
distance(kaliningrad,	740).
distance(zhytomyr,		0).
distance(kishinev,		362).
distance(niznovgorod,	1223).
distance(spb,			1080).
distance(brest,			404).
distance(viciebsk,		558).
distance(volgograd,		1160).
distance(donetsk,		712).
distance(ufa,			1906).
distance(moscow,		855).
distance(murmansk,		2097).
distance(minsk,			407).
distance(kazan,			1496).
distance(oryol,			594).
distance(voronezh,		755).
distance(yaroslavl,		1100).

rout(vilnius,		brest,			531).
rout(viciebsk,		brest,			638).
rout(viciebsk,		vilnius,		360).
rout(voronezh,		viciebsk,		896).
rout(voronezh,		volgograd,		581).
rout(volgograd,		viciebsk,		1455).
rout(viciebsk,		niznovgorod,	911).
rout(vilnius,		daugavpils,		211).
rout(kaliningrad, 	brest,			699).
rout(kaliningrad, 	vilnius,		333).
rout(kaunas, 		vilnius,		102).
rout(kyiv, 			vilnius,		734).
rout(kyiv, 			zhytomyr,		131).
rout(zhytomyr, 		donetsk,		863).
rout(zhytomyr, 		volgograd,		1493).
rout(kishinev, 		kyiv,			467).
rout(kishinev,		donetsk,		812).
rout(spb, 			viciebsk,		602).
rout(spb, 			kaliningrad,	739).
rout(spb, 			riga,			641).
rout(moscow, 		kazan,			815).
rout(moscow, 		niznovgorod,	411).
rout(moscow, 		minsk,			690).
rout(moscow, 		donetsk,		1084).
rout(moscow, 		spb,			664).
rout(murmansk, 		spb,			1412).
rout(murmansk, 		minsk,			2238).
rout(oryol, 		viciebsk,		522).
rout(oryol, 		donetsk,		709).
rout(oryol, 		moscow,			368).
rout(odesa, 		kyiv,			487).
rout(riga, 			kaunas,			267).
rout(tallin, 		riga,			308).
rout(kharkiv, 		kyiv,			471).
rout(kharkiv, 		simferopol,		639).
rout(yaroslavl, 	voronezh,		739).
rout(yaroslavl, 	minsk,			940).
rout(ufa, 			kazan,			525).
rout(ufa, 			samara,			461).

target(zhytomyr).

move(A, B) :- rout(A,B,_);rout(B,A,_).

dfs_(From, To, _, [move(From, To)]) :-
	move(From, To),
	write(From -> ""),
	write(To).
dfs_(From, To, VisitedNodes, [(From, X)|TailPath]) :-
	write(From -> ""),
	move(From, X),
	not(member(X,VisitedNodes)),
	dfs_(X, To, [From|VisitedNodes], TailPath).

ddfs_(From, To, _, _, [move(From, To)]) :-
	move(From, To),
	write(From -> ""),
	write(To).
ddfs_(From, To, Counter, VisitedNodes, [(From, X)|TailPath]) :-
	write(From -> ""),
	C1 is Counter - 1,
	C1 > 1,
	move(From, X),
	not(member(X,VisitedNodes)),
	ddfs_(X, To, C1, [From|VisitedNodes], TailPath).

idfs_(From, To, Path, Counter) :-
	ddfs_(From, To, Counter,[],Path).
idfs_(From, To, Path, Counter) :-
	Count is Counter + 1,
	idfs_(From, To, Path, Count).

width_(From,_,_,_) :-
	%write(From -> ""),
	target(From),!.
width_(From,Queue,Used,[(From, To)|Tail]) :-
	move(From, To),
	not(member(To,Used)),
	append(Queue,[To],Queue1),
	append(Used,[To],Used1),
	width_(From,Queue1,Used1,Tail).
width_(_,[Head|Tail],Used,Path) :-
	write(Head --> ""),
	width_(Head,Tail,Used,Path).

both_sides_(Array, Brray, Path) :-
	member([X|Van],Array),
	member([X|Ban],Brray),
	reverse([X|Van],Var),
	append(Var,Ban,Path).
both_sides_([[A|Va]|OtherA], [[B|Vb]|OtherB], Path) :-
	findall(X,(move(A,X),not(member(X,[A|Va]))),CityesA),
	findall(X,(move(B,X),not(member(X,[B|Vb]))),CityesB),
	maplist(consed([A|Va]), CityesA, CityesA1),
	maplist(consed([B|Vb]), CityesB, CityesB1),
	append(OtherA, CityesA1, OtherA1),
	append(OtherB, CityesB1, OtherB1),
	both_sides_(OtherA1, OtherB1, Path).
consed(A, B, [B|A]).

eager_(Targer,_,[Targer|Path]) :- target(Targer).
eager_(CurNode,Vicited,[CurNode|Path]) :-
	findall([X, C], (move(CurNode, C),distance(C, X),not(member(C, Vicited))), T), 	% X is for distance, C is for city
	write(T),nl,
	write(Vicited),nl,
	list_min(T,[_,MinCity]),
	write(MinCity),nl,
	append(Vicited,[MinCity],NewOne),
	write(NewOne),nl,nl,
	eager_(MinCity,NewOne,Path).

both_sides(Path) :- longStory,both_sides_([[spb]],[[zhytomyr]],Path),!.
dfs(Path) :- longStory,dfs_(spb,zhytomyr,[],Path),!.
ddfs(Path,Limit) :- longStory,ddfs_(spb,zhytomyr,Limit,[],Path),!.
idfs(Path) :- longStory,idfs_(spb,zhytomyr,Path,0),!.
width(Path) :- longStory,width_(spb,[],[],Path),!.
eager(Path) :- longStory,eager_(spb,[spb],Path),!.

longStory :- set_prolog_flag(answer_write_options,[max_depth(0)]).

list_min([Result],Result).
list_min([[Numb,_],[Numb1,City1]|CitiesTail],Result) :-
	Numb >= Numb1,!,list_min([[Numb1,City1]|CitiesTail],Result).
list_min([[Numb,City],[Numb1,_]|CitiesTail],Result) :-
	Numb < Numb1,list_min([[Numb,City]|CitiesTail],Result).