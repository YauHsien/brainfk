
%% The case "Hello, world":
% >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++
% [<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.
% --------.>>>++++[<++++++++>-]<+.
case(
    " >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++
     [<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.
     --------.>>>++++[<++++++++>-]<+."
).


machine((pointer(0), cells([]))).

command(>, forward(1)).
command(>(N), forward(N)).
command(<, backward(1)).
command(<(N), backward(N)).
command(+, inc(1)).
command(+(N), inc(N)).
command(-, dec(1)).
command(-(N), dec(N)).
command('.', output(1)).
command('.'(N), output(N)).
command(',', input(1)).
command(','(N), input(N)).


eval(forward(N), (pointer(P), cells(Cs)), (pointer(P1), cells(Cs))) :-
    P1 is P+N.

eval(backward(N), (pointer(P), cells(Cs)), (pointer(P1), cells(Cs))) :-
    P1 is P-N.

eval(inc(N), (pointer(P), cells(Cs)), (pointer(P), cells([(P,V)|Cs2]))) :-
    once((member((P,V0), Cs), !; V0 = 0)),
    V is (V0+N) mod 256,
    findall((P2,V2), (member((P2,V2), Cs), P =\= P2), Cs2).

eval(dec(N), (pointer(P), cells(Cs)), (pointer(P), cells([(P,V)|Cs2]))) :-
    once((member((P,V0), Cs), !; V0 = 0)),
    V is (V0-N) mod 256,
    findall((P2,V2), (member((P2,V2), Cs), P =\= P2), Cs2).

eval(output(N), (pointer(P), cells(Cs)), Machine) :-
    N > 0, !,
    once((member((P,V), Cs), !, put_char(V); true)),
    N2 is N-1,
    eval(output(N2), (pointer(P), cells(Cs)), Machine).
eval(output(0), Machine, Machine).

eval(input(N), (pointer(P), cells(Cs)), Machine) :-
    N > 0, !,
    ( get_char(A), char_code(A,V),
      findall((P2,V2), (member((P2,V2), Cs), P =\= P2), Cs2)
    ),
    N2 is N-1,
    eval(input(N2), (pointer(P), cells([(P,V)|Cs2])), Machine).
eval(input(0), Machine, Machine).

%%%% To test ">++++++++", which means [forward(1), inc(8)],
% ?- machine(M), eval(forward(1), M, M2), eval(inc(8), M2, M3).
% M = (pointer(0), cells([])),
% M2 = (pointer(1), cells([])),
% M3 = (pointer(1), cells([(1, 8)])).


eval([], Machine, Machine) :- !.

eval([loop(Loop)|Prog], Machine, Machine2) :-
    \+ zerop(Machine), !,
    append(Loop, [loop(Loop)|Prog], Prog2),
    eval(Prog2, Machine, Machine2).
eval([loop(_)|Prog], Machine, Machine2) :- !,
    eval(Prog, Machine, Machine2).

eval([Term|Prog], Machine, Machine2) :-
    command(Term, Term2),
    eval(Term2, Machine, Machine4),
    eval(Prog, Machine4, Machine2).


zerop((pointer(P), cells(Cs))) :-
    once(\+ (member((P,V), Cs), \+ var(V), V =\= 0)).


inset(Term) :-
    member(Term, [>,<,+,-,'.',',','[',']']).


input(Raw, Parsed) :-
    string_codes(Raw, Cs0),
    findall(A, (member(E, Cs0),
                atom_codes(A, [E]),
                inset(A)), Raw0),
    parse(Raw0, Parsed).
parse(Raw0, Parsed) :-
    parse(Raw0, [[]], Parsed).

%%%% To test the input case "Hello, World",
% ?- case(Case), input(Case, Parsed), format("~p~n", [Parsed]).
% [>,+8,loop([<,+9,>,-]),<,'.',>,+4,loop([<,+7,>,-]),<,+,'.',+7,'.'(2),+3,'.',>(2),+6,loop([<,+7,>,-]),<,+2,'.',- 12,'.',>,+6,loop([<,+9,>,-]),<,+,'.',<,'.',+3,'.',- 6,'.',- 8,'.',>(3),+4,loop([<,+8,>,-]),<,+,'.']
% Case = " >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++\n     [<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.\n     --------.>>>++++[<++++++++>-]<+.",
% Parsed = [>, +8, loop([<, +9, >, -]), <, '.', >, +4, loop([...|...]), <|...].

parse([], [Acc], Parsed) :- !,
    condense_and_reverse(Acc, Parsed).
parse([], Incomplete, _) :- !,
    format("Failed parsing: ~p~n", [Incomplete]).
parse([']'|Raw0], [Part,Parent|Acc], Parsed) :- !,
    condense_and_reverse(Part, R0),
    parse(Raw0, [[loop(R0)|Parent]|Acc], Parsed).
parse(['['|Raw0], [Parent|Acc], Parsed) :- !,
    parse(Raw0, [[],Parent|Acc], Parsed).
parse([Term|Raw0], [Part|Acc], Parsed) :-
    parse(Raw0, [[Term|Part]|Acc], Parsed).
condense_and_reverse(List, Result) :-
    condense_and_reverse(List, [], Result).

condense_and_reverse([], Result, Result) :- !.
condense_and_reverse([Term|List], [], Result) :-
    inset(Term), !,
    condense_and_reverse(List, [Term], Result).
condense_and_reverse([X|List], [X|Acc], Result) :- !,
    double(X, X2),
    condense_and_reverse(List, [X2|Acc], Result).
condense_and_reverse([X|List], [Xn|Acc], Result) :-
    match(X,Xn, N),
    N2 is N+1,
    match(X, Xn2, N2), !,
    condense_and_reverse(List, [Xn2|Acc], Result).
condense_and_reverse([X|List], Acc, Result) :-
    condense_and_reverse(List, [X|Acc], Result).


double(>, >(2)).
double(<, <(2)).
double(+, +(2)).
double(-, -(2)).
double('.', '.'(2)).
double(',', ','(2)).

match(>, >(N), N).
match(<, <(N), N).
match(+, +(N), N).
match(-, -(N), N).
match('.', '.'(N), N).
match(',', ','(N), N).


example :-
    case(Case), input(Case, Prog), machine(M), eval(Prog, M, M2),
    format("~nEventual machine state: ~p~n", [M2]).
%%%% By running the example,
% ?- example.
% Helo, World!
% Eventual machine state: pointer(2),cells([(2,33),(3,0),(0,100),(1,87)])
% true.
