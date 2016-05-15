
-module(atom).
-vsn("string").
-on_load(func/arity).
-behaviour(Behaviour).
-compile()
-compile(inline_list_funcs)

-include()
-include_lib()
-defined() et al


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


pure, recursive, tailcall, costly, guardlike, throws
deprecated, public, inline


-export([function/arity, ...]).
-export_type([type/arity, ...]).
-export_record([name1, name2, ...]).


public foo() ->
    ok.
        
-type sandwhich() :: a | b | c | integer().

-record name{field1spec, field2spec, ...}.    





fix_path([], _)             -> ok
;       ([$/ | _], _)       -> ok
;       ([$: | Rest], Pre)  ->
            stuff
end

fix_path([Hd | Rest], Pre :: list)   ->
    fix_path((Rest, Pre ++ [Hd])
end

x = expr(), expr2(), expr3().


E = expression
        |> first(_1, 2)
        |> second(1, _1)
        |> multiple(a, _1, b, _1).

E = expression
        |> first(&1, 2)
        |> second(1, &1)
        |> multiple(a, &1, b, &1).
        

is_type(V1, V2, V3, ...)

[1, 2, 5..6]
[1, 3..9, 11]


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



case E of
;   P1 ->
;   P2 ->
;   P3 ->
;   else ->
else
    some(), expression()
end

ifguard
    G1 ->
;   G2 ->
;   G3 ->
else
    some(), expression()
end

if
;   E1 ->
;   E2 ->
;   E3 ->
else
    some(), expression()
end


case E1 of
    P1 ->
;   P2 ->
else case E2 of
;   P3 ->
;   P4 ->
else case E3 of
    P5 ->
;   P6 ->
else
    some(), expressions()
end


-enum(name:size/type,
     { id1
     , id2 [=N]
     }.


is_between(X, Min, Max)     ($tmp = X, $tmp >= Min, $tmp <= Max)


-spec foo( integer() ) -> #undefined | #greater | #lesser.
foo (X) when X < 0  -> #lesser
;   (X) when X > 0  -> #greater
;   (0)             -> #'some$weird$atom'
end

undefined <=> #undefined

boolean
binary
float
function(Arity)
function
integer
list
number
pid
port
record(name, size)                    
record(name)
reference
tuple
tuple(size)



module:records() -> [atom()]
module:record(size, atom() | tagtuple()) -> error() | pos_integer()
module:record(fields, atom() | tagtuple()) -> error() | [atom()]
module:record_field(types, atom() | [atom()], atom() | tagtuple()) -> error() | typeid() | [typeid()]
module:record_field(offset, atom() | [atom()], atom() | tagtuple()) -> error() | natural() | [natural()]



App
    Module
        Thing
        
App.Module:Thing()
Module.Thing()



need a first class string type