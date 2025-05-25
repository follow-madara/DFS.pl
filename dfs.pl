% DFS Implementation with Correct Test Cases

% Graph definition
edge(a, b).
edge(a, c).
edge(b, a).
edge(b, d).
edge(c, a).
edge(c, d).
edge(c, e).
edge(d, b).
edge(d, c).
edge(d, f).
edge(e, c).
edge(e, f).
edge(f, d).
edge(f, e).

% Get neighbors for a node
neighbors(Node, Neighbors) :-
    findall(N, edge(Node, N), Neighbors).

% Main DFS implementation
dfs(Start, Path) :-
    dfs_visit(Start, [], RevPath),
    reverse(RevPath, Path).

dfs_visit(Node, Visited, [Node|Visited]) :-
    neighbors(Node, Neighbors),
    all_visited(Neighbors, [Node|Visited]).

dfs_visit(Node, Visited, Path) :-
    neighbors(Node, Neighbors),
    member(Next, Neighbors),
    \+ member(Next, Visited),
    dfs_visit(Next, [Node|Visited], Path).

all_visited([], _).
all_visited([H|T], Visited) :-
    member(H, Visited),
    all_visited(T, Visited).

% Corrected test cases
test_dfs :-
    format('=== DFS Test Results ===~n~n'),
    
    % Test 1: Traversal from node 'a'
    (dfs(a, Path1) ->
        format('Test 1: Traversal from a~nPath: ~w~n', [Path1]),
        (sort(Path1, [a,b,c,d,e,f]) ->
            format('  PASS: All nodes visited~n~n')
        ;
            format('  FAIL: Not all nodes visited~n~n')
        )
    ;
        format('Test 1 FAILED~n~n')
    ),
    
    % Test 2: Traversal from node 'd'
    (dfs(d, Path2) ->
        format('Test 2: Traversal from d~nPath: ~w~n', [Path2]),
        (member(f, Path2) ->
            format('  PASS: Node f reached~n~n')
        ;
            format('  FAIL: Node f not reached~n~n')
        )
    ;
        format('Test 2 FAILED~n~n')
    ),
    
    % Test 3: Check for duplicates
    (dfs(b, Path3) ->
        length(Path3, L),
        sort(Path3, S),
        length(S, SL),
        (L =:= SL ->
            format('Test 3: No duplicates check~nPath: ~w~n  PASS: No duplicates found~n~n', [Path3])
        ;
            format('Test 3: No duplicates check~nPath: ~w~n  FAIL: Duplicates detected~n~n', [Path3])
        )
    ;
        format('Test 3 FAILED~n~n')
    ),
    
    format('=== Testing Complete ===~n').

% Example queries:
%  dfs(a, Path).   % Get path from node 'a'
%  test_dfs.       % Run all tests
