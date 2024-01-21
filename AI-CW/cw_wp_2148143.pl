% True if link L appears on A's wikipedia page
actor_has_link(L, A) :- 
    actor(A), wp(A, WT), wt_link(WT, L).

% keep track of visited oracles
:- dynamic visited/2.

% Initialize the visited list
initialize_visited(Agent) :-
    retractall(visited(A, _)),
    assert(visited(Agent, [])). 

% Update the visited list
update_visited(Agent, Oracle) :-
    visited(Agent, Visited),
    retract(visited(A,_)),
    assert(visited(Agent, [Oracle|Visited])).

% Using BFS to find the nearest oracle
eliminate(As, A, Agent) :- 
    As = [A], ! 
    ;
    visited(Agent, VisitedList),
    length(VisitedList, Length),
    Length >= 10, !, 
    (   As = [A] -> true
    ;   A = unknown, true
    )
    ;
    solve_task_multiple(o(K),_,Agent), !,
    update_visited(Agent, o(K)),
    agent_ask_oracle(Agent, o(K), link, L),
    include(actor_has_link(L), As, ViableAs),
    eliminate(ViableAs, A, Agent).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    my_agent(N),
    findall(Actor, actor(Actor), As),
    initialize_visited(N),
    eliminate(As, A, N).

% Find the nearest oracle using BFS, excluding visited oracles
solve_task_multiple(Obj,Cost,Agent) :-
    my_agent(Agent),
    get_agent_position(Agent, Pos),
    get_agent_energy(Agent, Energy),
    find_multiple_oracles(find(Obj), Pos, Energy, Path, Agent),
    length(Path, Cost).

% Check if there is a nearby oracle that has not been visited yet
achieved(Task, Pos, Agent) :-
    Task = find(Obj),
    map_adjacent(Pos, OraclePos, Obj),
    visited(Agent, VisitedList),
    \+ member(Obj, VisitedList).

% BFS search for the nearest charging station
nearest_charging_station(Task,P, Path,Agent) :-
    search_bfs(Task, [[P]], [], Path,Agent).

% Determine the energy threshold based on the grid size
determine_energy_threshold(N, Threshold) :-
    (   N >= 15, N =< 17 -> Threshold = 25
    ;   N >= 18, N =< 20 -> Threshold = 35
    ;   Threshold = 50
    ).

% Find the oracles
find_multiple_oracles(Task, StartPos, Energy, Path, Agent) :-
    ailp_grid_size(N),
    determine_energy_threshold(N, Threshold), % Determine the energy threshold based on the grid size
    (   Energy < Threshold ->
        % If energy is not enough
        nearest_charging_station(find(c(_)), StartPos, PathToCharger, Agent),
        agent_do_moves(Agent, PathToCharger),
        agent_topup_energy(Agent, c(_)),
        say("topup", Agent),
        last(PathToCharger, ChargingPos),
        % After charging, continue to find the oracle
        continue_task_from_position(Task, ChargingPos, PathFromCharger, Agent),
        append(PathToCharger, PathFromCharger, Path)
    ;   
        % If energy is enough
        search_bfs(Task, [[StartPos]], [], Path, Agent),
        agent_do_moves(Agent, Path)
    ).

% Continue the task from a given position
continue_task_from_position(Task, Pos, Path, Agent) :-
    (   Task = find(o(_)) ->
        search_bfs(Task, [[Pos]], [], Path, Agent),
        agent_do_moves(Agent, Path)
    ;   Path = []
    ).

search_bfs(Task, Queue, Visited, Path, Agent) :-
    search_bfs_check(Task, Queue, Visited, Path, Agent, true).

% BFS to search for the task whsile excluding visited oracles
search_bfs_check(Task, Queue, Visited, Path, Agent, IsInitialPos) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    % check_visited_length_and_oracles(Visited, ContinueSearch),
    (   IsInitialPos
    ->  edge_cases(Pos, Result), % Check the surroundings of the current position
        NewIsInitialPos = false
    ;   Result = continue,
        NewIsInitialPos = IsInitialPos
    ),
    (   Result = unknown -> writeln('unknown'), true
    ;   achieved(Task, Pos, Agent) -> reverse([Pos|RPath], [_|Path])
    ;   Result = continue,
        findall([NPos, Pos|RPath],
                (map_adjacent(Pos, NPos, empty),
                 \+ member(NPos, Visited),
                 \+ member([NPos|_], Rest)), 
                Newfound),
        append(Rest, Newfound, NewQueue),
        search_bfs(Task, NewQueue, [Pos|Visited], Path, Agent)
    ).

% Check if the visited list length and the number of oracles visited are sufficient
check_visited_length_and_oracles(Visited, ContinueSearch) :-
    length(Visited, Length),
    count_oracles_visited(Visited, OracleCount),
    (   Length >= 10, OracleCount >= 10
    ->  ContinueSearch = true
    ;   ContinueSearch = false
    ).

% Count the number of oracles visited
count_oracles_visited(Visited, Count) :-
    include(is_oracle, Visited, Oracles),
    length(Oracles, Count).

% check if a visited position is an oracle
is_oracle(o(_)).

% The edge cases
edge_cases(Pos, Result) :-
    findall(Obj, (map_adjacent(Pos, _, Obj), Obj \= empty, Obj \= a(_)), Objects),
    length(Objects, Number),
    analyze_objects(Objects, Result).

% Analyze the surrounding objects to determine the result
analyze_objects(Objects, Result) :-
    length(Objects, Number),
    (   Number= 4 -> Result = unknown, writeln('all positions blocked')
    ;   Result = continue
    ).

% % Find path to the task, considering energy levels and excluding visited oracles
% find_multiple_oracles(Task, StartPos, Energy, Path, A, VisitedList) :-
%     safe_energy_threshold(Threshold),
%     (   Energy > Threshold ->
%         % If energy is enough, find oracle while excluding visited ones
%         search_bf_exclude_oracles(Task, [[StartPos]], [], Path, VisitedList),
%         agent_do_moves(A, Path)
%     ;   % If energy is not enough, find charger first
%         search_for_nearest_charging_station(find(c(_)), StartPos, PathToCharger,VisitedList),
%         my_agent(A), agent_do_moves(A, PathToCharger),
%         agent_topup_energy(A, c(_)),
%         say("topup", A),
%         last(PathToCharger, ChargingPos),
%         % After charging, find oracle while excluding visited ones
%         search_bf_exclude_oracles(Task, [[ChargingPos]], [], PathFromCharger, VisitedList),
%         agent_do_moves(A, PathFromCharger),
%         append(PathToCharger, PathFromCharger, Path)
%     ).