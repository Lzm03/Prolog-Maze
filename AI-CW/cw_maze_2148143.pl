% % Solve the maze, aiming to get all the agents to p(N,N)
% solve_maze :-
%     my_agents(Agents),
%     find_moves(Agents,Moves),
%     agents_do_moves(Agents,Moves),
%     solve_maze.


% %%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% % Find a possible move for each agent
% find_moves([],[]).
% find_moves([A|As],[M|Moves]) :-
%     findall(P,agent_adjacent(A,P,_),PosMoves),
%     random_member(M,PosMoves),
%     find_moves(As,Moves).

% % 解决迷宫，目标是让所有智能体到达出口 p(N,N)
% solve_maze :-
%     my_agents(Agents),
%     maplist(init_agent_visited, Agents),
%     find_exit(Agents).

% % 初始化每个智能体的访问记录
% init_agent_visited(Agent) :-
%     retractall(visited(Agent, _)),
%     get_agent_position(Agent, Pos),
%     assert(visited(Agent, [Pos])).

% % 寻找出口
% find_exit([]).
% find_exit([A|As]) :-
%     ailp_grid_size(N),
%     get_agent_position(A, Pos),
%     (   Pos = p(N,N)
%     ->  leave_maze(A), find_exit(As) % 智能体已到达出口，继续下一个智能体
%     ;   find_moves(A, Move),
%         agents_do_moves([A], [Move]), % 移动智能体
%         find_exit([A|As]) % 继续尝试该智能体和其他智能体
%     ).

% find_moves(Agent, Move) :-
%     visited(Agent, Visited),
%     get_agent_position(Agent, CurrentPos),
%     findall(P, (agent_adjacent(Agent, P, empty), P \= CurrentPos, \+ member(P, Visited)), PosMoves),
%     (   PosMoves \= []
%     ->  random_member(Move, PosMoves), % 选择一个未访问的邻接点
%         update_visited(Agent, Move)
%     ;   backtrack(Agent, Visited, Move) % 如果所有邻接点都被访问过，进行回溯
%     ).


% % 更新智能体的访问记录
% update_visited(Agent, NewPos) :-
%     retract(visited(Agent, Visited)),
%     assert(visited(Agent, [NewPos|Visited])).

% % 回溯到上一个交叉点
% backtrack(Agent, Visited, Move) :-
%     reverse(Visited, ReversedVisited),
%     find_last_crossroad(ReversedVisited, Crossroad),
%     (   Crossroad \= []
%     ->  Move = Crossroad, % 返回最后一个交叉点的坐标
%         prune_visited(Agent, Crossroad) % 移除交叉点之后的访问记录
%     ;   Move = stay % 如果没有交叉点可回溯，则保持不动
%     ).


% % 寻找到达最后一个交叉点的路径
% find_last_crossroad(Visited, Crossroad) :-
%     find_crossroad_path(Visited, [], Crossroad).

% find_crossroad_path([], _, []).
% find_crossroad_path([P|Ps], Acc, Crossroad) :-
%     findall(Adj, (agent_adjacent(P, Adj, empty), \+ member(Adj, Acc)), Adjacents),
%     (   length(Adjacents, Len), Len > 1
%     ->  Crossroad = P % 如果找到交叉点，立即返回
%     ;   find_crossroad_path(Ps, [P|Acc], Crossroad) % 继续搜索
%     ).



% prune_visited(Agent, Crossroad) :-
%     visited(Agent, Visited),
%     prune_list(Visited, Crossroad, Pruned),
%     retract(visited(Agent, _)),
%     assert(visited(Agent, Pruned)).

% prune_list(Visited, Crossroad, Pruned) :-
%     append(Pruned, [Crossroad|_], Visited), !.



% 解决迷宫
solve_maze :-
    my_agents(Agents),
    maplist(init_agent_visited, Agents),
    find_exit(Agents).

% 初始化每个智能体的访问记录
init_agent_visited(Agent) :-
    retractall(visited(Agent, _)),
    get_agent_position(Agent, Pos),
    assert(visited(Agent, [Pos])).

% 对每个智能体寻找出口
find_exit([]).
find_exit([A|As]) :-
    ailp_grid_size(N),
    dfs(A, p(N,N)),
    find_exit(As).

% 深度优先搜索
dfs(Agent, Destination) :-
    get_agent_position(Agent, Pos),
    visited(Agent, Visited),
    (   Pos = Destination
    ->  leave_maze(Agent) % 找到出口，离开迷宫
    ;   findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, Visited)), PosMoves),
        (   PosMoves \= []
        ->  select_move_and_continue(Agent, PosMoves, Destination) % 选择移动并继续
        ;   backtrack_to_crossroad(Agent, Destination) % 没有移动，回溯
        )
    ).

% 选择一个移动并继续
select_move_and_continue(Agent, PosMoves, Destination) :-
    member(Move, PosMoves),
    update_visited(Agent, Move),
    agents_do_moves([Agent], [Move]),
    dfs(Agent, Destination).

% 更新访问记录
update_visited(Agent, NewPos) :-
    retract(visited(Agent, Visited)),
    assert(visited(Agent, [NewPos|Visited])).

% 回溯到上一个交叉点并从那里继续搜索
backtrack_to_crossroad(Agent, Destination) :-
    visited(Agent, Visited),
    (   backtrack_step(Agent, Visited)
    ->  dfs(Agent, Destination) % 如果成功回溯，继续搜索
    ;   fail % 如果无法回溯，搜索失败
    ).

% 回溯到上一个交叉点并从那里继续搜索
backtrack_to_crossroad(Agent,Destination) :-
    visited(Agent, Visited),
    backtrack_step(Agent, Visited),
    dfs(Agent, Destination).

backtrack_step(Agent, [CurrentPos, NextPos|Rest]) :-
    % agent_adjacent(Agent, NextPos, empty),
    findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, [NextPos|Rest])), PosMoves),
    (   length(PosMoves, Len), Len > 1
    ->   agents_do_moves([Agent], [CurrentPos])
    ;   agents_do_moves([Agent], [NextPos]),
        backtrack_step(Agent, [NextPos|Rest])
    ).
backtrack_step(_, [_]).


% backtrack_step(Agent, [PrevPos, NextPos|Rest]) :-
%     agent_adjacent(Agent, NextPos, empty),
%     findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, [NextPos|Rest])), PosMoves),
%     (   length(PosMoves, Len), Len > 1,
%     ->  agents_do_moves([Agent], [PrevPos]) 
%     ;   agents_do_moves([Agent], [NextPos]),
%         backtrack_step(Agent, [NextPos|Rest])
%     ).

% backtrack_step(_, [_]).

% % 回溯到上一个有选择的位置
% backtrack_step(Agent, Visited) :-
%     agent_adjacent(Agent, NextPos, empty),
%     findall(P, (agent_adjacent(Agent, P, empty), \+ memberchk(P, Visited)), PosMoves),
%     (   length(PosMoves,L), L > 1% 如果有未探索的邻接点
%     ->  true % 找到了新的可探索路径，停止回溯
%     ;   agents_do_moves([Agent], [NextPos]), % 否则，回溯到上一个位置
%         backtrack_step(Agent, [NextPos, PrevPos|Rest]) % 继续回溯
%     ).

% % 深度优先搜索
% dfs(Agent, Destination) :-
%     get_agent_position(Agent, Pos),
%     visited(Agent, Visited),
%     (   Pos = Destination
%     ->  leave_maze(Agent) % 找到出口，离开迷宫
%     ;   findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, Visited)), PosMoves),
%         (   PosMoves \= []
%         ->  select_move_and_continue(Agent, PosMoves, Destination) % 选择移动并继续
%         ;   backtrack_to_crossroad(Agent, Visited, Destination) % 没有移动，回溯
%         )
%     ).

% % 选择一个移动并继续
% select_move_and_continue(Agent, PosMoves, Destination) :-
%     member(Move, PosMoves), % 选择第一个可行的移动
%     update_visited(Agent, Move),
%     agents_do_moves([Agent], [Move]),
%     dfs(Agent, Destination).

% % 更新访问记录
% update_visited(Agent, NewPos) :-
%     retract(visited(Agent, Visited)),
%     assert(visited(Agent, [NewPos|Visited])).

% % 回溯到上一个交叉点并从那里继续搜索
% backtrack_to_crossroad(Agent, Visited, Destination) :-
%     backtrack_step(Agent, Visited,Destination),
%     dfs(Agent, Destination).

% % 回溯到上一个有选择的位置并从那里继续搜索
% backtrack_step(Agent, Visited, Destination) :-
%     writeln('Visited:'),
%     writeln(Visited), % 调试信息：输出反转后的访问记录
%     (   find_decision_point(Agent, Visited, NewVisited),
%         writeln('NewVisited:'),
%         writeln(NewVisited) % 调试信息：输出新的访问记录
%     ->  retract(visited(Agent, _)),
%         assert(visited(Agent, NewVisited)),
%         NewVisited = [NewPos|_],
%         agents_do_moves([Agent], [NewPos]), % 回溯到新位置
%         writeln('NewPos:'),
%         writeln(NewPos), % 调试信息
%         dfs(Agent, Destination)
%     ;   writeln('No path to backtrack'), % 没有更多路径可回溯
%         fail % 搜索失败
%     ).

% % 寻找决策点
% find_decision_point(_, [], []) :- !, fail. % 没有其他路径可选
% find_decision_point(Agent, [Pos|Rest], [Pos|Rest]) :-
%     findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, Rest)), AdjacentMoves),
%     writeln('AdjacentMoves'), % 调试信息：输出可行的移动
%     AdjacentMoves \= [], !. % 找到具有未探索选择的位置
% find_decision_point(Agent, [_|Rest], NewVisited) :-
%     find_decision_point(Agent, Rest, NewVisited). % 继续回溯

