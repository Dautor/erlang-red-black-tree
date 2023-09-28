
-module('rb').
-export([test/0, rbtree/0, add/2, delete/2, get/2, tree/1]).

get_(_, e) -> nil;
get_(P, {L,V,_,_}) when P <  V -> get_(P,L);
get_(P, {_,V,_,_}) when P == V -> P;
get_(P, {_,V,_,R}) when P >  V -> get_(P,R).

height_(e) -> 0;
height_({L,_,_,R}) -> max(height_(L), height_(R)) + 1.

to_black(e) -> e;
to_black({L,V,_,R}) -> {L,V,b,R}.

add_(P,N) -> to_black(insert(P,N)).
insert(P, e) -> {e, P, r, e};
insert(P, {L,V,C,R}) when P <  V -> fix(insert(P,L),V,C,R);
insert(P, {L,V,C,R}) when P == V -> {L,V,C,R};
insert(P, {L,V,C,R}) when P >  V -> fix(L,V,C,insert(P,R)).

fix({{LL,LV,r,LR},V,r,RL},RV,b,RR) -> {{LL,LV,b,LR},V,r,{RL,RV,b,RR}}; % LL
fix(LL,LV,b,{{LR,V,r,RL},RV,r,RR}) -> {{LL,LV,b,LR},V,r,{RL,RV,b,RR}}; % RL
fix({LL,LV,r,{LR,V,r,RL}},RV,b,RR) -> {{LL,LV,b,LR},V,r,{RL,RV,b,RR}}; % LR
fix(LL,LV,b,{LR,V,r,{RL,RV,r,RR}}) -> {{LL,LV,b,LR},V,r,{RL,RV,b,RR}}; % RR
fix(L,V,C,R) -> {L,V,C,R}. % black parent

black_({L,V,r,R}) -> {{L,V,b,R}, false};
black_(N)         -> {N,         true}.

delete_(P, N) -> {S,_} = del_(P,N), to_black(S).

del_(_, e) -> {e,false};
del_(P, {L,V,C,R}) when P < V ->
    {L_,D} = del_(P,L),
    if
        D    -> unbalanced_r(L_,V,C,R);
        true -> {{L_,V,C,R},false}
    end;
del_(P, {L,V,C,R}) when P > V ->
    {R_,D} = del_(P,R),
    if
        D    -> unbalanced_l(L,V,C,R_);
        true -> {{L,V,C,R_},false}
    end;
del_(P, {L,V,C,e}) when P == V ->
    if
        C == b -> black_(L);
        true   -> {L,false}
    end;
del_(P, {L,V,C,R}) when P == V ->
    {{R_,D},M} = delete_min(R),
    if
        D    -> unbalanced_l(L,M,C,R_);
        true -> {{L,M,C,R_},false}
    end.

balance_r(A,X,b,{B,Y,r,{C,Z,r,D}}) -> {{A,X,b,B},Y,r,{C,Z,b,D}};
balance_r(A,X,b,{{B,Y,r,C},Z,r,D}) -> {{A,X,b,B},Y,r,{C,Z,b,D}};
balance_r(L,V,C,R)                 -> {L,V,C,R}.
balance_l({{A,X,r,B},Y,r,C},Z,b,D) -> {{A,X,b,B},Y,r,{C,Z,b,D}};
balance_l({A,X,r,{B,Y,r,C}},Z,b,D) -> {{A,X,b,B},Y,r,{C,Z,b,D}};
balance_l(L,V,C,R)                 -> {L,V,C,R}.
unbalanced_l({LL,LV,b,LR},V,C,R)              -> {balance_l({LL,LV,r,LR},V,b,R), C==b};
unbalanced_l({LL,LV,r,{LRL,LRV,b,LRR}},V,b,R) -> {{LL,LV,b,balance_l({LRL,LRV,r,LRR},V,b,R)}, false}.
unbalanced_r(L,V,C,{RL,RV,b,RR})              -> {balance_r(L,V,b,{RL,RV,r,RR}), C==b};
unbalanced_r(L,V,b,{{RLL,RLV,b,RLR},RV,r,RR}) -> {{{balance_r(L,V,b,{RLL,RLV,r,RLR})},RV,b,RR}, false}.

delete_min({e,V,b,e})            -> {{e,true},V};
delete_min({e,V,b,{RL,RV,r,RR}}) -> {{{RL,RV,b,RR},false},V};
delete_min({e,V,r,R})            -> {{R,false},V};
delete_min({L,V,C,R}) ->
    {{L_,D},M} = delete_min(L),
    if
        D    -> {unbalanced_r(L_,V,C,R), M};
        true -> {{{L_,V,C,R},false},     M}
    end.

rbtree() -> rbtree(e).
rbtree(T) ->
    receive
        % modifications
        {S, {add,    P}} -> T1 = add_   (P, T), S!ok, rbtree(T1);
        {S, {delete, P}} -> T1 = delete_(P, T), S!ok, rbtree(T1);
        % querying
        {S, {get,    P}} -> S!get_   (P, T), rbtree(T);
        {S, {height}}    -> S!height_(T),    rbtree(T);
        {S, {tree}}      -> S!T,             rbtree(T);
        % error
        A -> io:format("~w~n", A), rbtree(T)
    end.

start_client(Y, X) -> Y!{self(), X}, receive Z -> Z end.

add   (Instance, Value) -> start_client(Instance, {add,    Value}).
delete(Instance, Value) -> start_client(Instance, {delete, Value}).
get   (Instance, Value) -> start_client(Instance, {get,    Value}).
tree  (Instance)        -> start_client(Instance, {tree}).
height(Instance)        -> start_client(Instance, {height}).

test() ->
    register(rb0, spawn('rb', rbtree, [])),
    delete(rb0, '6'),
    add(rb0, '1'),
    add(rb0, '5'),
    io:format("~w~n", [tree(rb0)]),
    add(rb0, '2'),
    add(rb0, '4'),
    add(rb0, '3'),
    add(rb0, '6'),
    io:format("~w~n", [tree(rb0)]),
    delete(rb0, '2'),
    delete(rb0, '3'),
    io:format("~w~n", [tree(rb0)]),
    delete(rb0, '5'),
    delete(rb0, '4'),
    io:format("~w~n", [tree(rb0)]),
    delete(rb0, '6'),
    delete(rb0, '1'),
    io:format("~w~n", [tree(rb0)]),
    io:format("~w~n", [height(rb0)]),
    io:format("~w~n", [get(rb0, '3')]),
    unregister(rb0),
    ok.
