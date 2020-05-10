goodN(N) :- N #> 0.

goodC(counts(A,B,C,D), N) :-
    length(A, N),
    length(B, N),
    length(C, N),
    length(D, N).

getCount([_], C) :-
    C #= 1.
getCount([A, B|T], C) :-
    A #> B,
    getCount([A|T], C).
getCount([A, B|T], C) :-
    A #< B,
    getCount([B|T], C-1).

getColumnU([], [], _).
getColumnU([H|T], [CH|CT], N) :-
    nth0(N, H, CH),
    getColumnU(T, CT, N).

getColumnD(T, C, N) :-
    getColumnU(T, U, N),
    reverse(U, C).

goodRowsL([], [], 0).
goodRowsL([H|T], [CLH|CLT], N) :-
    getCount(H, CLH),
    NN #= N - 1,
    goodRowsL(T, CLT, NN).

goodRowsR([], [], 0).
goodRowsR([H|T], [CLH|CLT], N) :-
    reverse(H, RH),
    getCount(RH, CLH),
    NN #= N - 1,
    goodRowsR(T, CLT, NN).

goodColumnU(_, [], N, P) :-
    N #= P.
goodColumnU(T, [CUH|CUT], N, Pr) :-
    getColumnU(T, CT, Pr),
    getCount(CT, CUH),
    Prr #= Pr+1,
    goodColumnU(T, CUT, N, Prr).

goodColumnD(_, [], N, P) :-
    N #= P.
goodColumnD(T, [CUH|CUT], N, Pr) :-
    getColumnD(T, CT, Pr),
    getCount(CT, CUH),
    Prr #= Pr+1,
    goodColumnD(T, CUT, N, Prr).

tower(N, T, C) :- 
    goodN(N),
    length(T, N),
    length(C, 4),
    nth0(2, C, CL),
    goodRowsL(T, CL, N),
    nth0(3, C, CR),
    goodRowsR(T, CR, N),
    nth0(0, C, CU),
    goodColumnU(T, CU, N, 0),
    nth0(1, C, CD),
    goodColumnD(T, CD, N, 0).

% goodColumnD([[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [1,2,3,4,5],
%           [3,5,2,1,4]],
%           C,
%           5, 0).

% getColumnD([[2,3,4,5,1],
%           [5,4,1,3,2],
%           [4,1,5,2,3],
%           [1,2,3,4,5],
%           [3,5,2,1,4]],
%           C,
%           0).