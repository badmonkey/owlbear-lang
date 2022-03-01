
type A = X | Y
type B = X | Z
type C = A | B   => C = X | Y | Z


var IDENT TYPE

defun IDENT
    params: [var]
end

-- new scope with new bindings
def ast::let extends ast::scope
    vars: [{var, ast::expr}]
    body: ast::statements
end

-- new scope with variables marked as unusable (must be last statement in a scope)
def ast::drop extends ast::scope
    unvars: [IDENT]
    body: ast::statements
end



let v1 = e1
    v2 = e2
    ...
end

drop v1
    ...
end


-- repeat atleast a finite number of times (bounded so is safe)
repeat N
    ...
end


-- loop until break (unbounded so is unsafe)
loop
    ...
    break
    ...
end

-- if elif else end
cond
    CLAUSE1 ... end
    CLAUSE2 ... end
    else ... end -- default is else error end
end

try
    ...
catch
    ...
end


-- for pattern matching?
typeswitch IDENT
    TYPE ... end
end



quote/unquote AST


-----------------------------------------
// complicated scope example

fn send_if() {
    let data = Thing()

    if some_test() {
        error
    }

    if some_condition(&data) {
        send_to_other_thread(data);     // <-- data is unusable after this point
        // drop data
    }

    if some_test2() {
        error
    }

    // end of scope destroy data?
}



-------------------------------------------
// object ref capabilities
https://blog.beardhatcode.be/2018/10/pony-capabilities.html

tag
===

no write, no read
can alias, can share


val
===
Immutable

no write
can read, can alias, can share


ref
===

no share
can read, can write, can alias


iso
===

no alias
can read, can write, can share


consume
=======

destructive read
remove an alias of an object

subtyping?

iso -> trn -> ref -> box -> tag
        +---> val ----^


recover
=======

increase restrictions?
can access any sendable (iso, val, tag) variables in enclosing scope

So you can recover an
* iso form {iso,trn,ref} (preserve mutability)
* val form {val,box} (preseve immutability)
* tag form {tag} (preserve opacity)



|                         | tag | box | val | ref | trn | iso |
|:------------------------|:---:|:---:|:---:|:---:|:---:|:---:|
| deny local read alias   |  y  |  y  |  y  |  y  |  y  |     |
| deny local write alias  |  y  |  y  |     |  y  |     |     |
| deny global read alias  |  y  |  y  |  y  |     |     |     |
| deny global write alias |  y  |     |     |     |     |     |



mutable
iso, trn, ref

immutable
val, box

opaque
tag


-----------------------------------------------------

Perceus takes a more aggressive approach where ownership of references is passed down into
each function: now map is in charge of freeing xs, and ys is
freed by print: no drop operations are emitted inside main as
all local variables are consumed by other functions

Transferring ownership, rather than retaining it, means
we can free an object immediately when no more references
remain. This both increases cache locality and decreases
memory usage. For map, the memory usage is halved: the list
xs is deallocated while the new list ys is being allocated.


fun fold(t : tree<k,a>, acc : b, f : (k, a, b) -> b) : b
  match t
    Node(_,l,k,v,r) -> r.fold( f(k,v,l.fold(acc,f)), f)
    Leaf            -> acc

val count = t.fold(0, fn(k,v,acc) if v then acc+1 else acc)

~>

fun spec-fold(t : tree<k,bool>, acc : int) : int
  match t
    Node(_,l,k,v,r) ->
      if unique(t) then { drop(k); free(t) } else { dup(l); dup(r) } // perceus inserted
      val x = if v then 1 else 0
      spec-fold(r, spec-fold(l,acc) + x)
    Leaf ->
      drop(t)
      acc

val count = spec-fold(t,0)

drop(x) := decrement reference count, free if we can
dup(x) := increment reference count
unique(x) := true if there are no other references


fun map(xs : list?a?, f : a -> e b) : e list?b?
    match xs
        Cons(x,xx) ->
            dup(x); dup(xx); val r = dropru(xs)
            Cons@r( dup(f)(x), map(xx,f))

dropru(x) := if unique(x) then x else NULL

fun map(xs : list?a?, f : a -> e b) : e list?b?
    match xs
        Cons(x,xx) ->
            val r = if unique(xs) then &xs
                    else dup(x); dup(xx); decref(xs); NULL
            Cons@r( dup(f)(x), map(xx,f))

decref(x) := decrement reference count ... but don't free?
