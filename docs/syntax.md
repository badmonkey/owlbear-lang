



body := <expr> <nl>
body := BEGIN <statements-expr> END

statements-expr := <statement-list> <expr>
statements-expr := <statement-list> error
statements-return := <statement-list> error
statements-return := <statement-list> return <expr>


== forms ==

struct <name>
    <name>: <type> = <expr>


datatype <name>
    case <name>
    case <name>(<type>,...)


public <name>(types): <type> @traits
def <name>(types): <type> @traits

<name>(<args>) ->
    <statements-expr>
assert
    <asserts>
using
    <function-forms>


public length(list): number [pure]

length([]) -> 0
length([_ | tail]) ->
    _l(tail, 1)
using
    def _l(list(T), number): number @pure
    _l([], N: number): number -> N
    _l([_ | tail], N: number): number -> _l(tail, N + 1)

    def _i(list(T), number): bool
    _i([], 0) -> error
    _i([], N) -> false
    _i([_ | tail], 0) -> true
    _i([_ | tail], N) -> _i(tail, N - 1)


== statements ==

assert <cond>

debug
    <statementlist>

let <var>: <type> = <expr>
let <pat> = <expr>
let? ...


guard <cond> else
guard let <var> = <expr> else
guard when <cond> that <cond> else
    <statements-return>

error

with <var> = <expr>
    <statements>

forever (<arg> = <expr>)*
    <statements-expr>



test() ->
    forever state = empty-state()
        receive
            {'+', X, Y} -> new-state(X + Y)
            {'*', X, Y} -> new-state(X - Y)
            otherwise -> new-state(.invalid)

test() ->
    let _state = empty-state()
    _forever(_state)
using
    _forever(state) -> _forever(_f(state))
    using
        _f(state) ->
            receive
                {'+', X, Y} -> new-state(X + Y)
                {'*', X, Y} -> new-state(X - Y)
                otherwise -> new-state(.invalid)




== expressions ==

receive     // process top message or wait timeout
    <pat> -> <body>
    otherwise -> <body>
after <timeout>
    <body>

select      // process messages in queue until a match or wait timeout
    <pat> -> <body>
after <timeout>
    <body>
catch
    <body>  //

cond
    <cond> -> <body>
otherwise -> <body>

case <expr>
    <pat> -> <body>
    <pat> when <cond> -> <body>
    _ -> <body>
otherwise
    <body>

try
    <statements-expr>
catch
    <statements-expr>
after
    <statements>

<expr> if <cond> else <expr>



yield <expr>
