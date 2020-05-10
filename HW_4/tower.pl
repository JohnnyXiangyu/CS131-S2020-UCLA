row_unique(Row) :-
	sort(Row, Sorted),
	length(Row, N_elements),
	length(Sorted, N_unique_elements),
	N_elements #= N_unique_elements.

checkRowUnique([]).
checkRowUnique([H|T]) :-
    row_unique(H),
    checkRowUnique(T).

getCount([_], 1).
getCount([A, B|T], C) :-
    A #> B,
    getCount([A|T], C).
getCount([A, B|T], C) :-
    A #< B,
    CC #= C - 1,
    getCount([B|T], CC).

getColumnU([], [], _).
getColumnU([H|T], [CH|CT], N) :-
    nth0(N, H, CH),
    getColumnU(T, CT, N).

getColumnD(T, C, N) :-
    getColumnU(T, U, N),
    reverse(U, C).

goodRowsL(_, 0, [], []).
goodRowsL(N, S, [H|T], [CLH|CLT]) :-
    length(H, N),
    fd_domain(H, 1, N),
    fd_all_different(H),
    getCount(H, CLH),
    NN is S - 1,
    goodRowsL(N, NN, T, CLT),
    fd_labeling(H).

goodRowsR(_, 0, [], []).
goodRowsR(N, S, [H|T], [CLH|CLT]) :-
    reverse(H, RH),
    length(RH, N),
    fd_domain(RH, 1, N),
    fd_all_different(H),
    getCount(RH, CLH),
    NN is S - 1,
    goodRowsR(N, NN, T, CLT),
    fd_labeling(H).

goodColumnU(_, [], N, P) :-
    N is P.
goodColumnU(T, [CUH|CUT], N, Pr) :-
    length(CT, N),
    fd_domain(CT, 1, N),
    fd_all_different(CT),
    getColumnU(T, CT, Pr),
    getCount(CT, CUH),
    Prr is Pr+1,
    goodColumnU(T, CUT, N, Prr),
    fd_labeling(CT).

goodColumnD(_, [], N, P) :-
    N is P.
goodColumnD(T, [CUH|CUT], N, Pr) :-
    length(CT, N),
    fd_domain(CT, 1, N),
    fd_all_different(CT),
    getColumnD(T, CT, Pr),
    getCount(CT, CUH),
    Prr is Pr+1,
    goodColumnD(T, CUT, N, Prr),
    fd_labeling(CT).

tower(N, T, counts(U, D, L, R)) :- 
    length(T, N),
    goodRowsL(N, N, T, L),
    goodRowsR(N, N, T, R),
    goodColumnU(T, U, N, 0),
    goodColumnD(T, D, N, 0).

sum_fd([], 0).
sum_fd([H|T], Sum) :-
    sum_fd(T, SumT),
    H + SumT #= Sum. % Operators on fd has a `#` prefix

question_fd(Lst) :-
    length(Lst, 5),
    fd_domain(Lst, 1, 5), % Numbers in Lst from [1, 5]
    fd_all_different(Lst),
    sum_fd(Lst, 15),
    fd_labeling(Lst). % Finally, pick up numbers from a set

% tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [3,5|[2,1,4]],
%           [1,2,3,4,5]],
%          counts([2,3,2,1,5], [4|[2,2,2,1]],
%                 [4,1,2,2,5],
%                 [2,4,2,2,1])).

% tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           Row3,
%           [RC41,5|Row4Tail],
%           Row5],
%          counts(Top, [4|BottomTail],
%                 [Left1,Left2,Left3,Left4,5],
%                 Right)).