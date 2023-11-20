% Accomplish a given Task and return the Cost
solve_task(Task, Cost) :-
    my_agent(A), 
    get_agent_position(A, P),
    get_agent_energy(A,Energy), % get initial energy (100) 
    (   achieved(Task, P) -> Path = [], Cost = 0
    ;   choose_search_method(Task, P, Path, Energy),
        agent_do_moves(A, Path),
        length(Path, Cost)).

% Two types of Tasks: 1-go 2-find
choose_search_method(Task, StartPos, Path, Energy) :-
    (   Task = go(Pos), 
        ground(Pos) -> search_a_star(Task, [0-[StartPos]], [], Path, Energy) % ground(Pos) -> if Pos is known do A* else do Bfs
    ;   search_bf(Task, [[StartPos]], [], Path)).

% Calculte the estimate cost h
heuristic(Pos, Task, H) :- 
    achieved(Task,Goal),
    map_distance(Pos, Goal, H).

% sort by key-value, e.g., [1-[a,b,...],3-[b,h,...]]
sort_queue(Queue, SortedQueue) :-
    keysort(Queue, SortedQueue).

enough_energy(CurrentPos, Task , Energy) :-
    achieved(Task, Goal),
    map_distance(CurrentPos, Goal, Distance),
    Energy >= Distance.

find_nearest_charging_station(Pos, NearestStation) :-
    findall(Station, map_adjacent(Pos, Station, c(_)), Stations),
    sort_by_distance(Pos, Stations, [NearestStation|_]).

distance_to_pos(Pos1,Pos2,Distance) :-
    map_distance(Pos1, Pos2, Distance).

sort_by_distance(Pos, Positions, SortedPositions) :-
    maplist(distance_to_pos(Pos), Positions, Distances),
    pairs_keys_values(Pairs, Distances, Positions),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedPositions).

    
% A* search
search_a_star(Task, [G-CurrentPath|RestQueue], Visited, Path, Energy) :-    
    CurrentPath = [CurrentPos|_],
    (   achieved(Task, CurrentPos) -> reverse(CurrentPath, [_|Path]) % startPos exclude from the path
    ;   findall(NewG-[Next|CurrentPath],
            (   map_adjacent(CurrentPos, Next, empty),
                \+ member(Next, Visited),
                G1 is G + 1,   % To calculate the actual cost, one cost move
                (
                    enough_energy(CurrentPos,Task,Energy) -> true
                ;   find_nearest_charging_station(CurrentPos,NearestStation),
                    Next = NearestStation,
                    my_agent(A),agent_topup_energy(A,Energy)
                ),
                heuristic(Next, Task, H), % To calculate the estimate cost between the currentPos and the NextObj
                NewG is G1 + H % To calculate the score (actual + estimate)
            ),
            NewPaths),
        append(RestQueue, NewPaths, Queue),
        sort_queue(Queue, SortedQueue), % sort the agenda using a score
        NewVisited = [CurrentPos|Visited],
        search_a_star(Task, SortedQueue, NewVisited, Path,Energy)
    ).

% BFS search
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
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ; Task=go(Pos).