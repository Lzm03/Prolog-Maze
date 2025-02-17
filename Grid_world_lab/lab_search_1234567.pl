

% BFS search
search_bf :-
    my_agent(A),
    get_agent_position(A,P),
    (complete(P) -> true
    ;otherwise   -> search_bf([[P]],[],Path),
                    agent_do_moves(A,Path)).

search_bf(Queue,Visited,Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (complete(Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> findall([NPo000s,Pos|RPath],(map_adjacent(Pos,NPos,empty),\+ member(NPos,Visited), \+ member([NPos|_],Rest)), Newfound),
                      append(Rest,Newfound,NewQueue),
                      search_bf(NewQueue,[Pos|Visited],Path)).
                      
% DFS search
search_df :-
    my_agent(A),
    get_agent_position(A,StartPos),
    search_df([StartPos],[],Path),
    agent_do_moves(A,Path).

search_df(Current,Visited,Path) :-
    Current = [Pos|RPath],
    complete(Pos),
    reverse([Pos|RPath],[_|Path]).

search_df(Current,Visited,Path) :-
    Current = [Pos|RPath],
    \+ complete(Pos),
    map_adjacent(Pos,NPos,empty),
    \+ member(NPos,Visited),
    search_df([NPos,Pos|RPath],[Pos|Visited],Path).

% Test if the objective has been completed at a given position
complete(Pos) :- map_adjacent(Pos,_,o(_)).

