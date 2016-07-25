
-module(atom).
-vsn("string").
-on_load(func/arity).
-behaviour(Behaviour).
-compile()
-compile(inline_list_funcs)


via aleppo
-define, -undef, -ifdef, -ifndef, -else, -endif, -include, -include_lib

-module(application.mod).
-module(baron.std.monod)
-warning("something")
-error("error message").
-import(baron.std.set).
-import(baron.std.set -> set).
-require(baron.std, application2).


-module baron.std.monod
-version "1.0.0/batman"
-warning "some warning"


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


-export([function/arity, ...]).
-export_type([type/arity, ...]).
-export_record([name1, name2, ...]).

-export(function/arity,arity,...).

-export(start/1,2).
-export([start/{1,2}, stop/1, reset/2]).
-export(start/1, start/2, stop/1, reset/2).
-export(start/1,2  stop/1  reset/2).
-export(start/1,2; stop/1; reset/2).
-export start/1,2  stop/1  reset/2.
-export [start/1, start/2, stop/1, reset/2].


public foo() ->
    ok.
        
-deftype sandwhich() :: a | b | c | integer().
-type sandwhich() :: a | b | c | integer().

-record name{field1spec, field2spec, ...}.    

-record dog
    { name  :: string()
    , age   :: integer()
    }.


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
    
    
guard is_between(X, Min, Max) ->
    % only one clause
    % only allow guard tests + expansions of guard functions.
end

can be expanded into guard test inside guard or called as function
params need to be relabel to avoid duplicate collisions


x = expr(), expr2(), expr3().


E = expression
        |> `first(&1, 2)
        |> `second(1, &1)
        |> `multiple(a, &1, b, &1).

E = expression
        |> fun first(&1, 2)/1
        |> fun second(1, &1)/1
        |> fun multiple(a, &1, b, &1)/1.



E = with fun maybe/1 expression |> first(&1, 2) |> ..
    with `maybe(&1) expression |> first(&1, 2) |> ..
    with fun maybe(&1)/1 do expression |> 
    
E = expression with fun maybe/1 |> first(&1, 2) |> ..
    expression with `maybe(&1) |> first(&1, 2) |> ..
    expression with fun maybe(&1)/1 |> 
       
    
    E |> F2 |> F3 |> F4 ...
    (((E |> F2) |> F3) |> F4) ...
    ... F4( F3( F2(E) ) )
    
    E with F1 |> F2 |> F3 |> F4 ...
    (((E with F1 |> F2) |> F3) |> F4) ...
    
    E with T |> F  => 'monad(T, F, E)
    
    'monad(F1, F2, E) with F1 |> F3 |> F4 ...
    'monad(F1, F3, 'monad(F1, F2, E)) with F1 |> F4 ...
    'monad(F1, F4, 'monad(F1, F3, 'monad(F1, F2, E))) with F1 ...
    
    
    'monad(T, N, E) ->
        case T(E) of
            true    -> N(E)
        ;   X       -> X
        end
        
    'Maybe(F1, E, fun(X1) ->
                    'Maybe(F1, X, fun(X2) ->
                                  end)
                  end)

chain function calls
Object:new(value):set_title():something().
        
        

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
;   otherwise ->
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


receive :: type() | exception()
;   clause -> body
;   clause -> body
else
    body
end


module:records() -> [atom()]
module:record(size, atom() | tagtuple()) -> error() | pos_integer()
module:record(fields, atom() | tagtuple()) -> error() | [atom()]
module:record_field(types, atom() | [atom()], atom() | tagtuple()) -> error() | typeid() | [typeid()]
module:record_field(offset, atom() | [atom()], atom() | tagtuple()) -> error() | natural() | [natural()]


fun() -> ... end

fun atom/num
fun module:atom/num
fun app.module:atom/num

fun NAME/num
fun NAME(params*)/num


App
    Module
        Thing
        
App.Module:Thing()
Module:Thing()

   
atoms
'atom$with#specials'

`atom
#atom
$atom
:atom

need a first class string type



record$set(#record{} = X, field, value) -> X#record{ field = calue }


like recless

-record r_address{ city :: string() }.
-record r_owner{ address :: r_address{} }.
-record r_project{ owner :: #r_owner{} }.

Project :: r_project{}


City = ((Project#r_project.owner)#r_owner.address)#r_address.city,
NewProject =
       Project#r_project{owner =
        (Project#r_project.person)#r_owner{address =
          ((Project#r_project.person)#r_owner.address)#r_address{city =
            'Boston'}}}.
            

City = Project.owner.address.city,
NewProject = Project.owner.address.city = 'Boston'.

NewProject = Project.owner.address#{city = "Boston"}
NewProject = Project.owner#{address = r_address()}
NewProject = Project.owner#{address = #r_address{city = "Boston"}}


record$set(Project, owner,
    record$set(Project.owner, address,
        record$set(Project.owner.address, city, "Boston") ) )

        

interface Monad
    bind()  -> X
;   unit()  -> X
end



baron$Application.Module
baron$Application.Module/Protocol

Appl std
module enum
  protocol enumerable
  end
end

baron.std.enum.beam
baron.std.enum#enumerable.beam


protocol enumerable
    -callback count( enumerable() ) -> {ok, non_neg_integer()} | {error, module()}.
    -callback member( enumerable(), term() ) -> {ok, boolean} | {error, module()}.
    -callback reduce( term(), acc(), reducer() ) -> result().
end

implement enumerable for list() with

    reduce(_,       {halt, Acc}, _Fun)      -> {halted, Acc}
    ;     (List,    {suspend, Acc}, Fun)    -> {suspended, Acc, fun reduce(List, &1, Fun)/1 }
    ;     ([],      {cont, Acc}, _Fun)      -> {done, Acc}
    ;     ([H | T], {cont, Acc}, Fun)       -> reduce(T, Fun(H, Acc), Fun)
    end

end


enum:map(Enum, Fun) ->
    Reducer = fun(X, Acc) -> {cont, [Fun(X) | Acc]} end,
    enumerable.reduce(Enum, {cont, []}, Reducer)
        |> fun element(&1, 2)/1
        |> fun lists.reverse
end

enum:all(Enum, Fun) when is_function(Fun, 1) ->
    enumerable.reduce( Enum, {cont, true}
                     , fun(Entry, _) ->
                            if
                                Fun(Entry)  -> {cont, true}
                            else
                                {halt, false}
                            end
                       end)
        |> fun({_, X}) -> X end
end

enum:count(Enum) ->
    case enumerable.count(Enum) of
        {ok, Value} when is_integer(Value) -> Value
        
    ;   {error, Module} ->
            Module:reduce( Enum, {cont, 0}
                         , fun(_, Acc) -> {cont, Acc + 1} end)
                |> fun element(&1, 2)/1
    end
  end

  
protocol mutatable extends enumerable
end
  
  
-----------------------------------------------------------------------------------

minmax(_, [])           -> undefined
;     (F, [H | Rest])   -> minmax(F, Rest, {H, H})
end


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


-----------------------------------------------------------------------------------


function(Function) -> io_lib:chars()
exprs(Expressions) -> io_lib:chars()
form(Form) -> io_lib:chars()

erl_pp:form(F)
    


------------------------------------------------------------------------------------
 

burn foo.brn
burn src/
burn src/foo.brn
burn app1


apps/
  app1/
    include/
    src/
    priv/
src/  

build/
  build.info
  app1/
    include
    ebin/
    src/
    priv/
  app2/
    include
    ebin/
    src/
    priv/
  relx.config
  Dockerfile
  release/
  ebin/
  src/





 
