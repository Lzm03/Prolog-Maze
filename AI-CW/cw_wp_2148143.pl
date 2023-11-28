% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).

% Attempt to solve by visiting each oracle in ID order
eliminate(As,A,K) :- 
    As=[A], !
    ;
    solve_task_multiple(find(o(K)),_), !,
    my_agent(N),
    agent_ask_oracle(N,o(K),link,L), 
    include(actor_has_link(L),As,ViableAs), 
    K1 is K+1, 
    eliminate(ViableAs,A,K1).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    findall(A,actor(A),As), eliminate(As,Result,1),
    (   Result = [] -> A = unknown
    ;   A = Result ).

safe_energy_threshold(50). % threshold 20

% Accomplish a given Task and return the Cost
solve_task_multiple(Task, Cost) :-
    my_agent(A),
    get_agent_position(A, P),
    get_agent_energy(A, Energy),
    (   achieved(Task, P) -> Path = [], Cost = 0
    ;   find_path(Task, P, Energy, Path, A)
    ),
    length(Path, Cost).

find_path(Task, StartPos, Energy, Path, A) :-
    safe_energy_threshold(Threshold),
    (   Energy > Threshold ->
        % if energy enough, find oracle
        search_bf(Task, [[StartPos]], [], Path),
        agent_do_moves(A, Path)
    ;   % if energy not enough, find charger first
        search_for_nearest_charging_station(find(c(_)), StartPos, PathToCharger),
        % Path from current position to charger
        my_agent(A), agent_do_moves(A, PathToCharger),
        agent_topup_energy(A, c(_)), % topup agent
        last(PathToCharger, ChargingPos),
        % Path from charger to next oracle
        search_bf(Task, [[ChargingPos]], [], PathFromCharger),
        agent_do_moves(A, PathFromCharger),
        append(PathToCharger, PathFromCharger, Path)
    ).

% BFS search for the nearest charging station
search_for_nearest_charging_station(Task, StartPos, Path) :-
    search_bf(Task, [[StartPos]], [], Path).

% BFS search for unknown position
search_bf(Task,Queue,Visited,Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path]) % startPos exclude from the path
    ;otherwise     -> findall([NPos,Pos|RPath],
                        (map_adjacent(Pos,NPos,empty),
                        \+ member(NPos,Visited), 
                        \+ member([NPos|_],Rest)), 
                         Newfound),
                      append(Rest,Newfound,NewQueue),
                      search_bf(Task,NewQueue,[Pos|Visited],Path)).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj).