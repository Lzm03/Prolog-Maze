% % True if link L appears on A's wikipedia page
% actor_has_link(L,A) :- 
%     actor(A), wp(A,WT), wt_link(WT,L).

% % Attempt to solve by visiting each oracle in ID order
% eliminate(As,A,K) :- 
%     As=[A], !
%     ;
%     solve_task_multiple(find(o(K)),_), !,
%     my_agent(N),
%     agent_ask_oracle(N,o(K),link,L), 
%     include(actor_has_link(L),As,ViableAs), 
%     K1 is K+1, 
%     eliminate(ViableAs,A,K1).

% % Deduce the identity of the secret actor A
% find_identity(A) :- 
%     findall(A,actor(A),As), eliminate(As,Result,1),
%     (   length(Result,1), [A] = Result
%     ;   A = unknown
%     ).

% safe_energy_threshold(50). % threshold 20

% % Accomplish a given Task and return the Cost
% solve_task_multiple(Task, Cost) :-
%     my_agent(A),
%     get_agent_position(A, P),
%     get_agent_energy(A, Energy),
%     (   achieved(Task, P) -> Path = [], Cost = 0
%     ;   find_path(Task, P, Energy, Path, A)
%     ),
%     length(Path, Cost).

% find_path(Task, StartPos, Energy, Path, A) :-
%     safe_energy_threshold(Threshold),
%     (   Energy > Threshold ->
%         % if energy enough, find oracle
%         search_bf(Task, [[StartPos]], [], Path),
%         agent_do_moves(A, Path)
%     ;   % if energy not enough, find charger first
%         search_for_nearest_charging_station(find(c(_)), StartPos, PathToCharger),
%         % Path from current position to charger
%         my_agent(A), agent_do_moves(A, PathToCharger),
%         agent_topup_energy(A, c(_)), % topup agent
%         say("topup", A), % topup agent
%         last(PathToCharger, ChargingPos),
%         % Path from charger to next oracle
%         search_bf(Task, [[ChargingPos]], [], PathFromCharger),
%         agent_do_moves(A, PathFromCharger),
%         append(PathToCharger, PathFromCharger, Path)
%     ).

% % BFS search for the nearest charging station
% search_for_nearest_charging_station(Task, StartPos, Path) :-
%     search_bf(Task, [[StartPos]], [], Path).

% % BFS search for unknown position
% search_bf(Task,Queue,Visited,Path) :-
%     Queue = [Next|Rest],
%     Next = [Pos|RPath],
%     (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path]) % startPos exclude from the path
%     ;otherwise     -> findall([NPos,Pos|RPath],
%                         (map_adjacent(Pos,NPos,empty),
%                         \+ member(NPos,Visited), 
%                         \+ member([NPos|_],Rest)), 
%                          Newfound),
%                       append(Rest,Newfound,NewQueue),
%                       search_bf(Task,NewQueue,[Pos|Visited],Path)).

% % True if the Task is achieved with the agent at Pos
% achieved(Task,Pos) :-  
%     Task=find(Obj), map_adjacent(Pos,_,Obj).


% 依赖的库谓词
:- dynamic known_actor_link/2.

% 初始化已知演员链接
init_known_actor_links :-
    findall((Actor, Link), (actor(Actor), wp(Actor, WT), wt_link(WT, Link)), Pairs),
    maplist(assertz, Pairs).

% True if link L appears on A's wikipedia page
actor_has_link(L,A) :-
    actor(A), wp(A,WT), wt_link(WT,L).

% Eliminate unlikely actors based on the link provided by oracle
eliminate(As,A,K) :-
    As=[A], !
    ;
    my_agent(Agent),
    agent_ask_oracle(Agent, o(K), link, L),
    include(actor_has_link(L), As, ViableAs),
    K1 is K+1,
    eliminate(ViableAs, A, K1).

% 计算两个位置之间的距离
distance(Pos1, Pos2, Distance) :-
    Pos1 = p(X1, Y1),
    Pos2 = p(X2, Y2),
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% 选择下一个目标神谕
choose_next_oracle(MyPos, Oracles, ChosenOracle, Path) :-
    findall(Dist-Oracle, (member(Oracle, Oracles), map_distance(MyPos, Oracle, Dist)), DistOracles),
    sort(DistOracles, SortedDistOracles),
    member(_-ChosenOracle, SortedDistOracles),
    solve_task(find(ChosenOracle), Path).

% 主要路径规划逻辑
plan_route(A, Oracles, Path) :-
    get_agent_position(A, MyPos),
    choose_next_oracle(MyPos, Oracles, NextOracle, PathToOracle),
    agent_check_oracle(A, NextOracle),
    plan_route(A, Oracles, RemainingPath),
    append(PathToOracle, RemainingPath, Path).

% Deduce the identity of the secret actor A
find_identity(A) :-
    init_known_actor_links,
    findall(Oracle, (map_adjacent(_, _, o(Oracle)), !), Oracles),
    my_agent(Agent),
    plan_route(Agent, Oracles, Path),
    follow_path_and_query_oracles(Agent, Path),
    findall(A, actor(A), As),
    eliminate(As, A, 1).

% 沿着路径移动并查询神谕
follow_path_and_query_oracles(_, []).
follow_path_and_query_oracles(A, [NextPos|Path]) :-
    move_agent_to(A, NextPos),
    (adjacent_to_oracle(NextPos, Oracle) ->
        query_oracle(A, Oracle);
        true),
    follow_path_and_query_oracles(A, Path).

% 移动代理到指定位置
move_agent_to(A, Pos) :-
    get_agent_energy(A, Energy),
    map_distance(Pos, NextPos, Distance),
    Energy >= Distance,
    agent_do_moves(A, [NextPos]).

% 与相邻的神谕交互
query_oracle(A, Oracle) :-
    agent_ask_oracle(A, Oracle, link, Link),
    update_known_actor_links(Link).

% 更新已知演员链接
update_known_actor_links(Link) :-
    known_actor_link(Actor, Link),
    retractall(known_actor_link(Actor, _)),
    assertz(known_actor_link(Actor, Link)).
