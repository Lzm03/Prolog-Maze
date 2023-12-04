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
    ;   findall(P, (agent_adjacent(Agent, P, empty), \+ memberchk(P, Visited)), PosMoves),
        (   PosMoves \= []
        ->  (   member(Move, PosMoves),
                update_visited(Agent, Move),
                agents_do_moves([Agent], [Move]),
                dfs(Agent, Destination)
            )
        ;   backtrack_to_crossroad(Agent)
        )
    ).

% 更新访问记录
update_visited(Agent, NewPos) :-
    retract(visited(Agent, Visited)),
    assert(visited(Agent, [NewPos|Visited])).

% 回溯到上一个交叉点并从那里继续搜索
backtrack_to_crossroad(Agent) :-
    visited(Agent, Visited),
    backtrack_step(Agent, Visited),
    % 重新开始DFS
    dfs(Agent, Destination).

backtrack_step(Agent, [PrevPos, NextPos|Rest]) :-
    agent_adjacent(Agent, NextPos, empty),
    findall(P, (agent_adjacent(Agent, P, empty), \+ member(P, [NextPos|Rest])), PosMoves),
    (   length(PosMoves, L), L > 1
    ->  agents_do_moves([Agent], [PrevPos]),  % 移动到交叉点并继续搜索
        writeln(PrevPos)
    ;   agents_do_moves([Agent], [NextPos]),
        writeln(NextPos),
        backtrack_step(Agent, [NextPos|Rest])
    ).
backtrack_step(_, [_]).
