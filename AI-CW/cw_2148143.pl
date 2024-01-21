solve_task(Task, Cost) :-
    my_agent(A),  
    get_agent_position(A, P),  
    (   achieved(Task, P) -> true
    ;   solve_Two_type_Tasks(Task, P, Path), 
        agent_do_moves(A, Path),
        length(Path, Cost)
    ).

% Chooses the appropriate search method based on the task (go or find)
solve_Two_type_Tasks(Task, StartPos, Path) :-
    (   Task = go(Pos), 
        write('A* search'),
        ground(Pos) -> initial_a_star_search(Task, [0-0-[StartPos]], [], Path) % ground(Pos) means chack if the Pos is available
    ;   write('BFS search'),  % othercase, BFS search for find task
        search_bfs(Task, [[StartPos]], [], Path) 
    ).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ; Task=go(Pos).

% BFS search for unknown position(find task)
search_bfs(Task,Queue,Visited,Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path]) % startPos exclude from the path beacuse it is the current position
    ;otherwise     -> findall([NPos,Pos|RPath],
                        (map_adjacent(Pos,NPos,empty),
                        \+ member(NPos,Visited), 
                        \+ member([NPos|_],Rest)), 
                         Newfound),
                      append(Rest,Newfound,NewQueue),
                      search_bfs(Task,NewQueue,[Pos|Visited],Path)).

% Initiate A* search with an agenda (To get the FinalPath)
initial_a_star_search(Task, InitialAgenda, Processed, FinalPath) :-
    a_star(Task, InitialAgenda, Processed, [], FinalPath),
    FinalPath \= [].

initial_a_star_search(Task, _, _, FinalPath) :-
    my_agent(A),
    get_agent_position(A, P),
    search_bfs(Task, [[P]], [], FinalPath).

% main A* search
a_star(Task, [CurrentBest|RemainingAgenda], VisitedList, VisitedPath, Path) :-
    my_agent(Agent),
    get_agent_energy(Agent, Energy),
    CurrentBest = _-CostToHere-[CurrentPosition|PreviousPath], % CurrentBest = Fcost-Hcost-Path
    check_next_step(Task, CurrentPosition, Energy, CostToHere,Agent, RemainingAgenda, VisitedList, VisitedPath, Path).

a_star(Task, [], _, _, _) :-
    my_agent(A),
    get_agent_position(A, P),
    search_bfs(Task, [[P]], [], Path).

% check the next step
check_next_step(Task, Position, Energy, Cost, Agent, Agenda, VisitedList, VisitedPath, FinalPath) :-
    (   % If enough energy
        has_enough_energy(Position, Task, Energy) 
    ->  process_current_position(Task, Position, Cost, Agent, Agenda, VisitedList, VisitedPath, FinalPath)
    ;   % If not enough energy
        find_nearest_charger(Position, Agent, Task, FinalPath)
    ).

% Process the current position
process_current_position(Task, Position, Cost, Agent, Agenda, VisitedList, VisitedPath, Path) :-
    (   achieved(Task, Position)  ->  reverse([Position|VisitedPath], [_|FinalPath]), Path = FinalPath
    ;   newnode(Position, Cost, Task, VisitedList, NextPos),
        append_Agenda(NextPos, Agenda, UpdatedAgenda),
        sort_Angenda(UpdatedAgenda, SortedAgenda),
        a_star(Task, SortedAgenda, [Position|VisitedList], [Position|VisitedPath], Path)
    ).

% Use BFS to find the nearest charger station
find_nearest_charger(CurrentPos, Agent, Task, Path) :-
    search_bfs(find(c(_)), [[CurrentPos]], [], Path),
    my_agent(A),agent_do_moves(A, Path),agent_topup_energy(A, c(_)),say(topup, A),get_agent_position(A, NewStartPos),
    initial_a_star_search(Task, [0-0-[NewStartPos]], [], Path).

% check if there is enough energy to reach the destination
has_enough_energy(CurrentPos, Task , Energy) :-
    achieved(Task, Goal),
    map_distance(CurrentPos, Goal, Distance),
    Energy >= Distance.

% distance from current position to target
heuristic(Pos, Task, H) :- 
    achieved(Task,Goal),
    map_distance(Pos, Goal, H).

% Expands the current agenda
newnode(CurrentPos, G, Task, VisitedList, Expansions) :-
    find_possible_nextPos(CurrentPos, G, Task, VisitedList, Expansions).

% find all possible Next possition from the current position
find_possible_nextPos(CurrentPos, G, Task, VisitedList, NextPos) :-
    findall(
        F-H-NewG-[Next|CurrentPath],
        calculate_nextPos(CurrentPos, Next, VisitedList, G, Task, F, H, NewG),
        NextPos
    ).

% calculate the F, H, and G values for the next position
calculate_nextPos(CurrentPos, Next, Visited, G, Task, F, H, NewG) :-
    map_adjacent(CurrentPos, Next, empty),
    is_not_obstacle(CurrentPos, Next),
    \+ member(Next, Visited),
    heuristic(Next, Task, H),
    NewG is G + 1,
    F is NewG + H.

% check if the next position is not an obstacle
is_not_obstacle(CurrentPos, Next) :-
    \+ map_adjacent(CurrentPos, Next, t(_)).

% append nextPos to the agenda
append_Agenda(NextPos, Agenda, NewAgenda) :-
    foldl(insert_Agenda, NextPos, Agenda, NewAgenda).

% insert next possible Position to the agenda
insert_Agenda(NextPos, Agenda, [NextPos|Agenda]).

% Self-Setting sorting function to sort the agenda based on F, H, and G values
sort_Angenda(Agenda, SortedAgenda) :-
    predsort(compare_three_cost, Agenda, SortedAgenda).

% Compares two positions based on their F, H, and G values for sorting
compare_three_cost(Sequence, TotalCost1-HeuristicCost1-CurrentCost1-_, TotalCost2-HeuristicCost2-CurrentCost2-_) :-
    (   TotalCost1 = TotalCost2  % one situation, if the total cost is the same, then compare H and G values
    ->  compare_hg_cost(Sequence, HeuristicCost1, CurrentCost1, HeuristicCost2, CurrentCost2)
    ;   compare(Sequence, TotalCost1, TotalCost2) % other situation, compare total cost,choose the smaller one
    ).

% Further compare H and G values
compare_hg_cost(Sequence, HeuristicCost1, CurrentCost1, HeuristicCost2, CurrentCost2) :-
    (   HeuristicCost1 = HeuristicCost2 % if H values are the same, then compare G values,choose the larger one
    ->  compare(Sequence, CurrentCost1, CurrentCost2)
    ;   compare(Sequence, HeuristicCost1, HeuristicCost2) % other situation, compare H values, choose the smaller one
    ).


