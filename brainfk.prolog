
%% The case "Hello, world":
% >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++
% [<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.
% --------.>>>++++[<++++++++>-]<+.
case(
    [
        >, +(8),
        loop([
            <, +(9), >, -
        ]),
        >, '.', <, +(4),
        loop([
            <, +(7), >, -
        ]),
        <, +, '.', +(7), '.'(2), +(3), '.', >(2), +(6),
        loop([
            <, +(7), >, -
        ]),
        <, +(2), '.', -(12), '.', >, +(6),
        loop([
            <, +(9), >, -
        ]),
        <, +, '.', <, '.', +(3), '.',
        -(5), '.', -(8), '.', +(3), +(4),
        loop([
            <, +(8), >, -
        ]),
        <, +, '.'
    ]
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
    V is V0+N,
    findall((P2,V2), (member((P2,V2), Cs), P =\= P2), Cs2).

eval(dec(N), (pointer(P), cells(Cs)), (pointer(P), cells([(P,V)|Cs2]))) :-
    once((member((P,V0), Cs), !; V0 = 0)),
    V is V0-N,
    findall((P2,V2), (member((P2,V2), Cs), P =\= P2), Cs2).

eval(output(N), (pointer(P), cells(Cs)), (pointer(P), cells(Cs))) :-
    N > 0, !,
    once((member((P,V), Cs), !, put_char(V); true)).
eval(output(0), Machine, Machine).

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

%%%% To test the case "Hello, World",
% ?- machine(M), case(Prog), eval(Prog,M,M2).
% elo, Worme͡
% M = (pointer(0), cells([])),
% Prog = [>, +8, loop([<, +9, >, -]), >, '.', <, +4, loop([...|...]), <|...],
% M2 = (pointer(-1), cells([(-1, 865), (0, 0), (1, 87), (2, 0)])).


zerop((pointer(P), cells(Cs))) :-
    once(\+ (member((P,V), Cs), \+ var(V), V =\= 0)).
