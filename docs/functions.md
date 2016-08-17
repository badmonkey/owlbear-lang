


name(Nparams) -> expr .

name(NparamsA) ->

;   (NparamsN) ->

end


minmax(_, []) -> undefined.
minmax(F, [H | Rest]) -> minmax(F, Rest, {H, H}).


minmax(_, [], Acc) -> Acc
;     (F, [H | Rest], {Min, Max} = Res)
            when is_function(F,2)  ->
    Acc =   case F(H, Min) of
                true -> {H, Max}
            else case F(Max, H) of
                true -> {Min, H}
            else
                Res
            end,
    minmax(F, Rest, Acc)
end



minmax(_, [], Acc) -> Acc
;     (F, [H | Rest], {Min, Max} = Res)
            when is_function(F,2)  ->
    Acc =   ifcond
            ;   F(H, Min) -> {H, Max}
            ;   F(Max, H) -> {Min, H}
            ;   else      -> Res
            end,
    minmax(F, Rest, Acc)
end





-spec fix_path( list(), list() ) -> ok | list().
-trait(fix_path/2, [pure, ~costly]).
-deprecated(fix_path/2).

fix_path([], _)             -> ok
...
end


-spec fix_path( list(), list() ) -> ok | list() [pure, throws].

@pure @~recursive @costly
fix_path([], _)             -> ok
...
end

-trait

-attribute(something).


pure, constant, recursive, tailcall, costly, guardlike, throws
deprecated, public, inline




-spec[pure, throws] fix_path(list(), list()) -> result.
-type[pure, throws] fix_path(list(), list()) -> result.
-fun[pure, throws] fix_path(list(), list()) -> result.
-funtype[pure, throws] fix_path(list(), list()) -> result.

-type[pure, throws](list(), list()) -> result.
fix_path([], _)             -> ok

-type(other_args) -> result.
;       ([$/ | _], _)       -> ok
;       ([$: | Rest], Pre)  ->
            stuff
end

fix_path([Hd | Rest], Pre :: list)   ->
    fix_path(Rest, Pre ++ [Hd])
end

fix_path([Hd | Rest], Pre) ->
    fix_path(Rest, Pre ++ [Hd]).



product(X :: integer, Y :: integer) ->
;      (X :: integer, Y :: list) ->
;      (X :: tuple, Y :: function) ->
;      (X :: tuple(3), Y :: function(3)) ->
;      (X :: tuple(N), Y :: function(N)) ->
;      (X :: function(N), Y :: tuple(N)) ->
end

product(X, Y) when is_integer(X, Y) ->
product(X, Y) when is_integer(X), is_list(Y) ->
product(X, Y) when is_tuple(X), is_function(Y) ->
product(X, Y) when is_tuple(X), 3 = tuple_size(X), is_function(Y, 3) ->
product(X, Y) when is_tuple(X), N = tuple_size(X), is_function(Y, N) ->
product(X, Y) when is_tuple(Y), N = tuple_size(Y), is_function(X, N) ->



fun() -> ... end

fun atom/num
fun module:atom/num
fun app.module:atom/num


fun NAME(V1, ..., Vm)/N

fun(P1, ..., PN) ->
	NAME(V1, ..., Vm)
end


fun func(&1, y)/1




    
defguard is_between(X, Min, Max) ->
    % only one clause
    % only allow guard tests + expansions of guard functions.
    % no recursion
    % functions that can be converted to guard expressions
end

can be expanded into guard test inside guard or called as function
params need to be relabel to avoid duplicate collisions

defmodule numbers
	public guard is_between(X, Min, Max) ->  X >= Min andalso X <= Max.
end

somefunc(X) when numbers:is_between(X, 1, 100) -> ...


also is_type(V1, V2, V3, ...)


