solve_maze :-
    my_agents(Agents),
    maplist(init_agent_state, Agents),
    find_exit(Agents).

% Finds the exit
find_exit([]).
find_exit([A|As]) :-
    ailp_grid_size(N),
    dfs_search(A, p(N,N)),
    find_exit(As).

% initial every dynamic list (visited, crossroads, deadends)
init_agent_state(Agent) :-
    retractall(visited(Agent, _)),
    retractall(crossroads(Agent, _)),
    retractall(deadends(Agent, _)),
    get_agent_position(Agent, Pos),
    assert(visited(Agent, [Pos])),
    assert(crossroads(Agent, [])),
    assert(deadends(Agent, [])).

% Depth-first search to find the exit
dfs_search(Agent, Goal) :-
    get_agent_position(Agent, Pos),
    visited(Agent, Visited),
    (   Pos = Goal
    ->  leave_maze(Agent) 
    ;   next_move(Agent, Pos, Visited, PosMoves),
        (   PosMoves = []
        ->  clear_visited(Agent), 
            find_and_update_deadend(Agent, Pos),
            dfs_search(Agent, Goal)
        ;   select_move(Agent, PosMoves, Goal)
        )
    ).

% Determines the next move
next_move(Agent, Pos, Visited, PosMoves) :-
    deadends(Agent, DeadEnds),
    findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, Visited), \+ member(P, DeadEnds)), PosMoves),
    update_positions(Agent, Pos, PosMoves).

% Checks if the current position is a deadend
is_deadend(Agent, Pos) :-
    deadends(Agent, DeadEnds),
    member(Pos, DeadEnds).

% Updates the agent's position information, including crossroads and deadends
update_positions(Agent, Pos, PosMoves) :-
    length(PosMoves, Len),
    (   Len > 2
    ->  update_crossroads(Agent, Pos) 
    ;   Len = 0
    ->  true 
    ;   true
    ).

% Using DFS to find and update deadends
find_and_update_deadend(Agent, Pos) :-
    crossroads(Agent, Crossroads),
    clear_visited(Agent),
    backtrack_dfs(Agent, Pos, Crossroads, [Pos], Path),
    update_deadend_from_path(Agent, Path),
    append(Path, OldVisited, NewVisited),
    update_visited_path(Agent, NewVisited).

% backtracking using DFS 
backtrack_dfs(Agent, CurrentPos, Crossroads, Visited, Path) :-
    (   member(CurrentPos, Crossroads)
    ->  Path = [CurrentPos]
    ;   deadends(Agent, DeadEnds),
        findall(NextPos, 
            (   agent_adjacent(Agent, NextPos, empty), 
                \+ member(NextPos, Visited),
                \+ member(NextPos, DeadEnds)
            ), 
            Neighbors),
        (   Neighbors \= []
        ->  member(Move, Neighbors),
            agents_do_moves([Agent], [Move]),
            update_visited(Agent, Move),
            backtrack_dfs(Agent, Move, Crossroads, [Move|Visited], NewPath),
            Path = [CurrentPos|NewPath]
        ;   Path = []
        )
    ).

% Updates the visited points
update_visited(Agent, NewPos) :-
    retract(visited(Agent, Visited)),
    assert(visited(Agent, [NewPos|Visited])).

% Updates the visited path
update_visited_path(Agent, Path) :-
    retract(visited(Agent, _)),
    assert(visited(Agent, Path)).

% Updates the deadend list 
update_deadend_from_path(Agent, Path) :-
    (   Path = [_,DeadEnd|_]
    ->  update_deadends(Agent, DeadEnd)
    ;   true
    ).

% Updates the list of dead ends
update_deadends(Agent, DeadEnd) :-
    retract(deadends(Agent, DeadEnds)),
    (   \+ member(DeadEnd, DeadEnds)
    ->  assert(deadends(Agent, [DeadEnd|DeadEnds]))
    ;   assert(deadends(Agent, DeadEnds))
    ).

% Clears the visited list after encountering a deadend
clear_visited(Agent) :-
    retractall(visited(Agent, _)),
    assert(visited(Agent, [])).

% Updates the list of crossroads
update_crossroads(Agent, Pos) :-
    retract(crossroads(Agent, Crossroads)),
    (   \+ member(Pos, Crossroads)
    ->  assert(crossroads(Agent, [Pos|Crossroads])),writeln('Crossroads:'),writeln([Pos|Crossroads])
    ;   assert(crossroads(Agent, Crossroads))
    ).

% Selects a move and continues the search
select_move(Agent, PosMoves, Goal) :-
    get_agent_position(Agent, Pos),
    (   has_all_directions(PosMoves)
    ->  select_right_move(Pos, PosMoves, Move)
    ;   select_preferred_move(Pos, PosMoves, Move)
    ),
    update_visited(Agent, Move),
    agents_do_moves([Agent], [Move]),
    dfs_search(Agent, Goal).

% Checks if four empty condition is met
has_all_directions(PosMoves) :-
    length(PosMoves, Len),
    Len >= 4.

% Selects a move based on priority (right, down, others)
select_preferred_move(Pos, PosMoves, Move) :-
    (   select_right_move(Pos, PosMoves, RightMove)
    ->  Move = RightMove
    ;   select_down_move(Pos, PosMoves, DownMove)
    ->  Move = DownMove
    ;   member(Move, PosMoves)
    ).

% Selects a move to the right
select_right_move(p(X, Y), PosMoves, p(RightX, Y)) :-
    RightX is X + 1,
    member(p(RightX, Y), PosMoves).

% Selects a move downward
select_down_move(p(X, Y), PosMoves, p(X, DownY)) :-
    DownY is Y + 1,
    member(p(X, DownY), PosMoves).
