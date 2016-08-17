

-warning("something")
-error("error message").
-require(baron.std, application2).




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
       
               

[1, 2, 5..6]
[1, 3..9, 11]


-enum(name:size/type,
     { id1
     , id2 [=N]
     }.



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



need a first class string type

        

interface Monad
    bind()  -> X
;   unit()  -> X
end




protocol enumerable
    -callback count( enumerable() ) -> {ok, non_neg_integer()} | {error, module()}.
    -callback member( enumerable(), term() ) -> {ok, boolean} | {error, module()}.
    -callback reduce( term(), acc(), reducer() ) -> result().
end

implement enumerable for list() with

    count(L) -> 
        length(L)
    end

    member(L, I) ->
        lists:ismemeber(I, L)
    end

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



baron list --public application[.module]
baron start application

 
