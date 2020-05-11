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

fd_colUnique([], []).
fd_colUnique([H|T], [CH|CT]) :- % vertically check each column
    H #\= CH,
    fd_colUnique(T, CT).
fd_accUnique([], _).
fd_accUnique([H|T], Challenger) :- % make sure each column doesn't contain duplicates
    fd_colUnique(H, Challenger),
    fd_accUnique(T, Challenger).

goodRows(_, [], [], [], _).
goodRows(N, [H|T], [CLH|CLT], [CRH|CRT], Acc) :-
    length(H, N),
    fd_domain(H, 1, N),
    fd_all_different(H),
    getCount(H, CLH),
    reverse(H_reverse, H),
    getCount(H_reverse, CRH),
    fd_accUnique(Acc, H),
    goodRows(N, T, CLT, CRT, [H|Acc]),
    fd_labeling(H).

% goodRowsR(_, 0, [], []).
% goodRowsR(N, S, [H|T], [CLH|CLT]) :-
%     reverse(H, RH),
%     length(RH, N),
%     fd_domain(RH, 1, N),
%     fd_all_different(H),
%     getCount(RH, CLH),
%     NN is S - 1,
%     goodRowsR(N, NN, T, CLT),
%     fd_labeling(H).

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
    goodRows(N, T, L, R, []),
    % goodRowsR(N, N, T, R),
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

plain_goodRowsL(_, [], []).
plain_goodRowsL(N, [H|T], [CLH|CLT]) :-
    length(H, N),
    plain_getCount(H, CLH),
    % NN is S - 1,
    plain_goodRowsL(N, T, CLT).

plain_goodRowsR(_, [], []).
plain_goodRowsR(N, [H|T], [CLH|CLT]) :-
    reverse(H, RH),
    length(RH, N),
    plain_getCount(RH, CLH),
    % NN is S - 1,
    plain_goodRowsR(N, T, CLT).

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

countRow(Row, L_count, R_count) :-
    plain_getCount(Row, L_count),
    reverse(Row, H_reverse),
    plain_getCount(H_reverse, R_count).

generateT([], _, [], [], _).
generateT([H|T], N, [LH|LT], [RH|RT], Acc) :- % generate a possible solution
    permuteRow(N, H),
    countRow(H, LH, RH),
    accUnique(Acc, H),
    append(Acc, [H], NewAcc),
    generateT(T, N, LT, RT, NewAcc).

plain_tower(N, T, counts(U, D, L, R)) :- 
    length(T, N),
    generateT(T, N, L, R, []),
    NN is N-1,
    checkUniqueColumns(T, NN),
    % plain_goodRowsL(N, T, L),
    % plain_goodRowsR(N, T, R),
    plain_goodColumnU(T, U, N, 0),
    plain_goodColumnD(T, D, N, 0).

ambiguous(N, C, T1, T2) :-
    tower(N, C, T1),
    tower(N, C, T2).
