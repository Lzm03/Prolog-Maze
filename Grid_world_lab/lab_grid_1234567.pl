% True if A is a possible movement direction
m(north).
m(east).
m(south).
m(west).

% True if p(X,Y) is on the board
on_board(p(X,Y)) :- ailp_grid_size(N),between(1,N,X), between(1,N,Y).

% True if p(X1,Y1) is one step in direction M from p(X,Y) (no bounds check)
pos_step(p(X,Y), north, p(X,Y1)) :- Y1 is Y - 1.
pos_step(p(X,Y), south, p(X,Y1)):- Y1 is Y + 1.
pos_step(p(X,Y), east, p(X1,Y)):- X1 is X + 1.
pos_step(p(X,Y), west, p(X1,Y)):- X1 is X - 1.

% True if NPos is one step in direction M from Pos (with bounds check)
new_pos(Pos,M,NPos) :- pos_step(Pos, M , NPos), on_board(NPos), on_board(Pos).

% True if a L has the same length as the number of squares on the board
complete(L) :- ailp_grid_size(N),Cells is N*N,length(L,Cells). % is the same with ==

% Perform a sequence of moves creating a spiral pattern, return the moves as L

turn(north,east).
turn(east,south).
turn(south,west).
turn(west,north).

spiral(Path,_Direction) :- 
    complete(Path),!,
    reverse(Path,[_|RPath]),
    my_agent(A),
    agent_do_moves(A,RPath).
    
spiral([Current|Path],Direction) :- 
    new_pos(Current, Direction, Next),
    \+ member(Next,Path),!,
    spiral([Next,Current|Path],Direction).

spiral([Current|Path],Direction) :- 
    turn(Direction,NewDirection),
    new_pos(Current, NewDirection, Next),
    \+ member(Next,Path),
    spiral([Next,Current|Path],NewDirection).

% % define a clockwise (ccw) turn
% turn(north,east,clockwise).
% turn(east,south,clockwise).
% turn(south,west,clockwise).
% turn(west,north,clockwise).
% % define an anticlockwise (acw) turn
% turn(D1,D2,anticlockwise) :- turn(D2,D1,clockwise).
% % Base case: spiral complete
% spiral(Ps,Qs,_) :- complete(Ps), !, reverse(Ps,Qs).
% % Recursive case: continue straight or turn
% spiral([Q,P|Ps],Qs,S) :- new_pos(P,D,Q), (C=D ; turn(D,C,S)), new_pos(Q,C,R), \+ member(R,[P|Ps]), spiral([R,Q,P|Ps],Qs,S).
% % Wrapper predicate
% spiral(Ps) :- my_agent(A), get_agent_position(A,P), new_pos(P,_,Q), spiral([Q,P],Ps,_).
% % Example query
% ?- spiral([_|Ps]), my_agent(A), agent_do_moves(A,Ps).
