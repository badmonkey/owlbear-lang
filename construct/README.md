
language design to implemnt state machines

A deterministic finite-state machine or deterministic finite-state acceptor is a
quintuple (EVENT, STATE, INIT, TRANS, FINAL), where:

EVENT is a finite non-empty set of symbol(args)
STATE is a finite non-empty set of symbol(data)
INIT is an initial state, an element of STATE;
TRANS is the state-transition function:
  TRANS(STATE, EVENT) -> STATE
FINAL is the set of final states, a (possibly empty) subset of STATE.


datatype Inputs
    case output(string)
    case plus(integer)
    case minus(integer)

datatype Bank
    case credit(integer)
    case debt(integer)
    case halt(string)


machine MACHINE(EVENT) : STATE = INIT

on EVENT
    when STATE
        ...


machine Test1(Inputs) : Bank = credit(0)

on plus(n)
    when credit(X)
        return credit(X+n)

    when debt(X)
        if n >= X then
            return credit(n-X)
        return debt(X-n)


on plus(n)
    when credit(X) -> credit(X+n)
    when debt(X) ->
        ifcond
            n >= X -> credit(n-X)
            otherwise -> debt(x-N)


---------------------------------------------------

machine MACHINE(EVENT) : STATE = INIT

state STATE
    on EVENT -> STATE
       statements


machine Test1(Inputs) : Bank -> credit(0)

state credit(X)
    on plus(n)
        return credit(X+n)

    on minus(n)
        guard n <= X else
            return debt(n-X)

        return credit(X-n)


state credit(X)
    on plus(n) -> credit(X+n)

    on minus(n) when n <= X -> credit(X-n)

    on minus(n) otherwise -> debt(n-X)



---------------------------------------------------


var foo : Test1 = Test1()
foo.plus(10)
foo.minus(10)



---------------------------------------------------

self: STATE
handle(self: STATE, evt: EVENT): STATE
    ...



self = handle(self, event)
