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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain_getCount([_], C) :-
    C = 1.
plain_getCount([A, B|T], C) :-
    A > B,
    plain_getCount([A|T], C).
plain_getCount([A, B|T], C) :-
    A < B,
    plain_getCount([B|T], CC),
    C is CC + 1.

plain_goodRowsL(_, 0, [], []).
plain_goodRowsL(N, S, [H|T], [CLH|CLT]) :-
    length(H, N),
    plain_getCount(H, CLH),
    NN is S - 1,
    plain_goodRowsL(N, NN, T, CLT).

plain_goodRowsR(_, 0, [], []).
plain_goodRowsR(N, S, [H|T], [CLH|CLT]) :-
    reverse(H, RH),
    length(RH, N),
    plain_getCount(RH, CLH),
    NN is S - 1,
    plain_goodRowsR(N, NN, T, CLT).

plain_goodColumnU(_, [], N, P) :-
    N is P.
plain_goodColumnU(T, [CUH|CUT], N, Pr) :-
    length(CT, N),
    getColumnU(T, CT, Pr),
    plain_getCount(CT, CUH),
    Prr is Pr+1,
    plain_goodColumnU(T, CUT, N, Prr).

plain_goodColumnD(_, [], N, P) :-
    N is P.
plain_goodColumnD(T, [CUH|CUT], N, Pr) :-
    length(CT, N),
    getColumnD(T, CT, Pr),
    plain_getCount(CT, CUH),
    Prr is Pr+1,
    plain_goodColumnD(T, CUT, N, Prr).

row_unique(Row) :-
	sort(Row, Sorted),
	length(Row, N_elements),
	length(Sorted, N_unique_elements),
	N_elements == N_unique_elements.

checkUniqueColumns(_, -1).
checkUniqueColumns(T, N) :-
    getColumnU(T, CT, N),
    row_unique(CT),
    NN is N-1,
    checkUniqueColumns(T, NN).

permute([], []).
permute([X|L], R) :- 
	permute(L, PL),
	append(PL1, PL2, PL),
    append(PL1, [X|PL2], R).    

pred(1, [1]).
pred(N, [N|T]) :-
    N > 1,
    N1 is N-1,
    pred(N1, T).

permuteRow(N, Row) :-
    pred(N, Src),
    permute(Src, Row).

colUnique([], []).
colUnique([H|T], [CH|CT]) :- % vertically check each column
    H \= CH,
    colUnique(T, CT).

accUnique([], _).
accUnique([H|T], Challenger) :- % make sure each column doesn't contain duplicates
    colUnique(H, Challenger),
    accUnique(T, Challenger).

generateT([], _, _).
generateT([H|T], N, Acc) :- % generate a possible solution
    permuteRow(N, H),
    accUnique(Acc, H),
    append(Acc, [H], NewAcc),
    generateT(T, N, NewAcc).

plain_tower(N, T, counts(U, D, L, R)) :- 
    length(T, N),
    generateT(T, N, []),
    NN is N-1,
    checkUniqueColumns(T, NN),
    plain_goodRowsL(N, N, T, L),
    plain_goodRowsR(N, N, T, R),
    plain_goodColumnU(T, U, N, 0),
    plain_goodColumnD(T, D, N, 0).


% plain_tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [3,5|[2,1,4]],
%           [1,2,3,4,5]],
%          counts([2,3,2,1,5], [4|[2,2,2,1]],
%                 [4,1,2,2,5],
%                 [2,4,2,2,1])).

% plain_tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [1,2,3,4,5],
%           [3,5,2,1,4]],
%          C).

% plain_tower(5, T,
%          counts([2,3,2,1,4],
%                 [3,1,3,3,2],
%                 [4,1,2,5,2],
%                 [2,4,2,1,2])).

% plain_tower(4, T,
%          counts([4,1,2,2],
%                 [1,4,2,2],
%                 [2,3,2,1],
%                 [3,1,2,2])).

% tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           Row3,
%           [RC41,5|Row4Tail],
%           Row5],
%          counts(Top, [4|BottomTail],
%                 [Left1,Left2,Left3,Left4,5],
%                 Right)).

% checkUniqueRows([[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [1,2,3,4,5],
%           [3,5,2,1,4]])

% checkUniqueColumns([[2,3,4,5,1],
%            [5,4,1,3,2],
%            [4,1,5,2,3],
%            [1,2,3,4,5],
%            [3,5,2,1,4]], 4)

% plain_tower(5,
%          [[2,3,4,5,1],
%           [5,4,1,3,2],
%           Row3,
%           [RC41,5|Row4Tail],
%           Row5],
%          counts(Top, [4|BottomTail],
%                 [Left1,Left2,Left3,Left4,5],
%                 Right)).

ambiguous(N, C, T1, T2) :-
    tower(N, C, T1),
    tower(N, C, T2).
