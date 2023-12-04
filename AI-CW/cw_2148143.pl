% Task = go or find
solve_task(Task, Cost) :-
    my_agent(A), 
    get_agent_position(A, P),
    (   achieved(Task, P) -> Path = [], Cost = 0
    ;   choose_search_method(Task, P, Path),
        agent_do_moves(A, Path),
        length(Path, Cost)).

% If the position is known, do A* search, else do BFS search
choose_search_method(Task, StartPos, Path) :-
    (   Task = go(Pos), 
        write('A* search'),
        ground(Pos) -> search_a_star(Task, [0-[StartPos]], [], Path)
    ;   
        write('BFS search'),
        search_bf(Task, [[StartPos]], [], Path)).

% cost = distance from current position to target
heuristic(Pos, Task, H) :- 
    achieved(Task,Goal),
    map_distance(Pos, Goal, H).

% keysort will sort by key, e.g., [1-[a,b,...],3-[b,h,...]] -> [1-[a,b,...],3-[b,h,...]]
sort_queue(Queue, SortedQueue) :-
    keysort(Queue, SortedQueue).

% check if there is enough energy to reach the destination
enough_energy(CurrentPos, Task , Energy) :-
    achieved(Task, Goal),
    map_distance(CurrentPos, Goal, Distance),
    Energy >= Distance.

% BFS search for the nearest charging station
nearest_charging_station(Task,StartPos, Path) :-
    search_bf(Task, [[StartPos]], [], Path).

% A* search for the known target
search_a_star(Task, [G-CurrentPath|RestQueue], Visited, Path) :-    
    CurrentPath = [CurrentPos|_],
    (   achieved(Task, CurrentPos) -> reverse(CurrentPath, [_|Path])
    ;   findall(NewG-[NewNext|NewPath],
            (   
                    my_agent(A), get_agent_energy(A, Energy),
                (
                    enough_energy(CurrentPos, Task, Energy) -> 
                    map_adjacent(CurrentPos, Next, empty),
                    \+ member(Next, Visited),
                    NewNext = Next,
                    heuristic(Next, Task, H),
                    NewPath = CurrentPath
                ;   
                    nearest_charging_station(find(c(_)), CurrentPos, NextStationPath),
                    last(NextStationPath, Next2),
                    NewNext = Next2, 
                    heuristic(Next2, Task, H),
                    my_agent(A), agent_do_moves(A, NextStationPath), agent_topup_energy(A, c(_)), % topup agent
                    say("topup", A)
                ),
                G1 is G + 1,
                NewG is G1 + H 
            ),
            NewPaths),
        append(RestQueue, NewPaths, Queue),
        NewVisited = [CurrentPos|Visited],
        sort_queue(Queue, SortedQueue),
        search_a_star(Task, SortedQueue, NewVisited, Path)
    ).

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
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ; Task=go(Pos).