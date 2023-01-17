// Tests ported from Simplesub

:ShowRelativeLineNums



// --- basic --- //


42
//│ res: 42

x => 42
//│ res: anything -> 42

x => x
//│ res: 'a -> 'a

x => x 42
//│ res: (42 -> 'a) -> 'a

(x => x) 42
//│ res: 42

f => x => f (f x)  // twice
//│ res: ('a -> 'b & 'c -> 'a) -> 'c -> 'b

let twice = f => x => f (f x)
//│ twice: ('a -> 'b & 'c -> 'a) -> 'c -> 'b



// --- booleans --- //


true
//│ res: true

not true
//│ res: bool

x => not x
//│ res: bool -> bool

(x => not x) true
//│ res: bool

x => y => z => if x then y else z
//│ res: bool -> (forall 'a. 'a -> (forall 'b. 'b -> ('a | 'b)))

x => y => if x then y else x
//│ res: (bool & 'a) -> (forall 'b. 'b -> ('a | 'b))

:e
succ true
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.+1: 	succ true
//│ ║        	^^^^^^^^^
//│ ╟── reference of type `true` is not an instance of type `int`
//│ ║  l.+1: 	succ true
//│ ╙──      	     ^^^^
//│ res: error | int

:e
x => succ (not x)
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.+1: 	x => succ (not x)
//│ ║        	     ^^^^^^^^^^^^
//│ ╟── application of type `bool` is not an instance of type `int`
//│ ║  l.+1: 	x => succ (not x)
//│ ║        	           ^^^^^
//│ ╟── but it flows into argument with expected type `int`
//│ ║  l.+1: 	x => succ (not x)
//│ ╙──      	          ^^^^^^^
//│ res: bool -> (error | int)

:e
(x => not x.f) { f: 123 }
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.+1: 	(x => not x.f) { f: 123 }
//│ ║        	^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── integer literal of type `123` is not an instance of type `bool`
//│ ║  l.+1: 	(x => not x.f) { f: 123 }
//│ ║        	                    ^^^
//│ ╟── Note: constraint arises from argument:
//│ ║  l.+1: 	(x => not x.f) { f: 123 }
//│ ║        	          ^^^
//│ ╟── from field selection:
//│ ║  l.+1: 	(x => not x.f) { f: 123 }
//│ ╙──      	           ^^
//│ res: bool | error

:e
(f => x => not (f x.u)) false
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.+1: 	(f => x => not (f x.u)) false
//│ ║        	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── reference of type `false` is not a function
//│ ║  l.+1: 	(f => x => not (f x.u)) false
//│ ║        	                        ^^^^^
//│ ╟── Note: constraint arises from application:
//│ ║  l.+1: 	(f => x => not (f x.u)) false
//│ ║        	                ^^^^^
//│ ╟── from reference:
//│ ║  l.+1: 	(f => x => not (f x.u)) false
//│ ╙──      	                ^
//│ res: {u: anything} -> bool | error



// --- records --- //


x => x.f
//│ res: {f: 'f} -> 'f

// note: MLsub returns "⊤" (equivalent)
{}
//│ res: anything

{ f: 42 }
//│ res: {f: 42}

{ f: 42 }.f
//│ res: 42

(x => x.f) { f: 42 }
//│ res: 42

f => { x: f 42 }.x
//│ res: (42 -> 'x) -> 'x

f => { x: f 42, y: 123 }.y
//│ res: (42 -> anything) -> 123

if true then { a: 1, b: true } else { b: false, c: 42 }
//│ res: {b: bool}

:e
{ a: 123, b: true }.c
//│ ╔══[ERROR] Type mismatch in field selection:
//│ ║  l.+1: 	{ a: 123, b: true }.c
//│ ║        	                   ^^
//│ ╟── record of type `{a: 123, b: true}` does not have field 'c'
//│ ║  l.+1: 	{ a: 123, b: true }.c
//│ ╙──      	^^^^^^^^^^^^^^^^^^^
//│ res: error

:e
x => { a: x }.b
//│ ╔══[ERROR] Type mismatch in field selection:
//│ ║  l.+1: 	x => { a: x }.b
//│ ║        	             ^^
//│ ╟── record of type `{a: ?a}` does not have field 'b'
//│ ║  l.+1: 	x => { a: x }.b
//│ ╙──      	     ^^^^^^^^
//│ res: anything -> error



// --- self-app --- //


x => x x
//│ res: ('a -> 'b & 'a) -> 'b

res id
//│ res: 'a -> 'a


let f = (x => x + 1); {a: f; b: f 2}
//│ f: int -> int
//│ res: {a: int -> int, b: int}

x => x x x
//│ res: ('a -> 'a -> 'b & 'a) -> 'b

x => y => x y x
//│ res: ('a -> 'b -> 'c & 'b) -> 'a -> 'c

x => y => x x y
//│ res: ('a -> 'b -> 'c & 'a) -> 'b -> 'c

:e // Omega: causes divergence in first-class-polymorphic type inference, as expected
(x => x x) (x => x x)
//│ ╔══[ERROR] Cyclic-looking constraint while typing application; a type annotation may be required
//│ ║  l.+1: 	(x => x x) (x => x x)
//│ ║        	^^^^^^^^^^^^^^^^^^^^^
//│ ╟── ————————— Additional debugging info: —————————
//│ ╟── this constraint:  ‹∀ 0. (α368_370' -> α369_371')›  <:  α368_377    PolymorphicType  TypeVariable
//│ ╙──  ... looks like:  ‹∀ 0. (α368_370' -> α369_371')›  <:  α368_370'
//│ res: error


x => {l: x x, r: x }
//│ res: ('a -> 'b & 'a) -> {l: 'b, r: 'a}


// * From https://github.com/stedolan/mlsub
// * Y combinator:
:e // similarly to Omega
(f => (x => f (x x)) (x => f (x x)))
//│ ╔══[ERROR] Cyclic-looking constraint while typing application; a type annotation may be required
//│ ║  l.+1: 	(f => (x => f (x x)) (x => f (x x)))
//│ ║        	      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── ————————— Additional debugging info: —————————
//│ ╟── this constraint:  ‹∀ 1. (α399_404'' -> α401_406'')›  <:  α399_415'    PolymorphicType  TypeVariable
//│ ╙──  ... looks like:  ‹∀ 1. (α399_404'' -> α401_406'')›  <:  α399_404''
//│ res: ('a -> 'a & 'a -> 'b) -> (error | 'b)

// * Z combinator:
:e
(f => (x => f (v => (x x) v)) (x => f (v => (x x) v)))
//│ ╔══[ERROR] Cyclic-looking constraint while typing application; a type annotation may be required
//│ ║  l.+1: 	(f => (x => f (v => (x x) v)) (x => f (v => (x x) v)))
//│ ║        	      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── ————————— Additional debugging info: —————————
//│ ╟── this constraint:  ‹∀ 1. (α460_475'' -> α469_479'')›  <:  α460_492'    PolymorphicType  TypeVariable
//│ ╙──  ... looks like:  ‹∀ 1. (α460_475'' -> α469_479'')›  <:  α460_475''
//│ res: (('a -> 'b) -> ('c -> 'd & 'a -> 'b) & ('c -> 'd) -> 'e) -> (error | 'e)

// * Function that takes arbitrarily many arguments:
:e
(f => (x => f (v => (x x) v)) (x => f (v => (x x) v))) (f => x => f)
//│ ╔══[ERROR] Cyclic-looking constraint while typing application; a type annotation may be required
//│ ║  l.+1: 	(f => (x => f (v => (x x) v)) (x => f (v => (x x) v))) (f => x => f)
//│ ║        	      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── ————————— Additional debugging info: —————————
//│ ╟── this constraint:  ‹∀ 1. (α563_578'' -> α572_582'')›  <:  α563_595'    PolymorphicType  TypeVariable
//│ ╙──  ... looks like:  ‹∀ 1. (α563_578'' -> α572_582'')›  <:  α563_578''
//│ res: 'a | error
//│   where
//│     'a :> anything -> 'a

res 1 2
//│ res: error | 'a
//│   where
//│     'a :> anything -> 'a


let rec trutru = g => trutru (g true)
//│ trutru: 'a -> nothing
//│   where
//│     'a <: true -> 'a

i => if ((i i) true) then true else true
//│ res: ('a -> true -> bool & 'a) -> true
// ^ for: λi. if ((i i) true) then true else true,
//    Dolan's thesis says MLsub infers: (α → ((bool → bool) ⊓ α)) → bool
//    which does seem equivalent, despite being quite syntactically different



// --- let-poly --- //


let f = x => x; {a: f 0, b: f true}
//│ f: 'a -> 'a
//│ res: {a: 0, b: true}

y => (let f = x => x; {a: f y, b: f true})
//│ res: 'a -> {a: 'a, b: true}

y => (let f = x => y x; {a: f 0, b: f true})
//│ res: ((0 | true) -> 'a) -> {a: 'a, b: 'a}

y => (let f = x => x y; {a: f (z => z), b: f (z => true)})
//│ res: 'a -> {a: 'a, b: true}

y => (let f = x => x y; {a: f (z => z), b: f (z => succ z)})
//│ res: (int & 'a) -> {a: 'a, b: int}



// --- recursion --- //


let rec f = x => f x.u
//│ f: 'u -> nothing
//│   where
//│     'u <: {u: 'u}


// from https://www.cl.cam.ac.uk/~sd601/mlsub/
let rec recursive_monster = x => { thing: x, self: recursive_monster x }
//│ recursive_monster: ('a & 'b) -> {self: 'c, thing: 'a}
//│   where
//│     'c :> {self: 'c, thing: 'b}



// --- random --- //


(let rec x = {a: x, b: x}; x)
//│ res: 'x
//│   where
//│     'x :> {a: 'x, b: 'x}

(let rec x = v => {a: x v, b: x v}; x)
//│ res: anything -> 'a
//│   where
//│     'a :> {a: 'a, b: 'a}

:e
let rec x = (let rec y = {u: y, v: (x y)}; 0); 0
//│ ╔══[ERROR] Type mismatch in binding of block of statements:
//│ ║  l.+1: 	let rec x = (let rec y = {u: y, v: (x y)}; 0); 0
//│ ║        	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── integer literal of type `0` is not a function
//│ ║  l.+1: 	let rec x = (let rec y = {u: y, v: (x y)}; 0); 0
//│ ║        	                                           ^
//│ ╟── Note: constraint arises from application:
//│ ║  l.+1: 	let rec x = (let rec y = {u: y, v: (x y)}; 0); 0
//│ ║        	                                    ^^^
//│ ╟── from reference:
//│ ║  l.+1: 	let rec x = (let rec y = {u: y, v: (x y)}; 0); 0
//│ ╙──      	                                    ^
//│ x: 0
//│ res: 0

(x => (let y = (x x); 0))
//│ res: ('a -> anything & 'a) -> 0

// TODO simplify more
(let rec x = (y => (y (x x))); x)
//│ res: (nothing -> 'a) -> 'a

res (z => (z, z))
//│ res: (nothing, nothing,)

next => 0
//│ res: anything -> 0

((x => (x x)) (x => x))
//│ res: 'a -> 'a

(let rec x = (y => (x (y y))); x)
//│ res: ('a -> 'b & 'a) -> nothing
//│   where
//│     'b <: 'b -> 'b

x => (y => (x (y y)))
//│ res: ('a -> 'b) -> (forall 'c. ('c -> 'a & 'c) -> 'b)

(let rec x = (let y = (x x); (z => z)); x)
//│ res: 'a -> 'a

(let rec x = (y => (let z = (x x); y)); x)
//│ res: 'a -> 'a

(let rec x = (y => {u: y, v: (x x)}); x)
//│ res: 'a
//│   where
//│     'a :> forall 'b. 'b -> {u: 'b, v: 'c}
//│     'c :> {u: 'a, v: 'c}

(let rec x = (y => {u: (x x), v: y}); x)
//│ res: 'a
//│   where
//│     'a :> forall 'b. 'b -> {u: 'c, v: 'b}
//│     'c :> {u: 'c, v: 'a}

(let rec x = (y => (let z = (y x); y)); x)
//│ res: 'x
//│   where
//│     'x :> forall 'a. 'a -> 'a
//│     'a <: 'x -> anything

(x => (let y = (x x.v); 0))
//│ res: ('v -> anything & {v: 'v}) -> 0

let rec x = (let y = (x x); (z => z)); (x (y => y.u)) // [test:T1]
//│ x: 'a -> 'a
//│ res: {u: 'u} -> 'u

:ns
let rec x = (let y = (x x); (z => z))
//│ x: forall 'x. 'x
//│   where
//│     'x := forall 'a. 'a -> 'a



// Converges under normal-order reduction, but type inference follows more of an applicative order:

:e
(w => x => x) ((y => y y) (y => y y))
//│ ╔══[ERROR] Cyclic-looking constraint while typing application; a type annotation may be required
//│ ║  l.+1: 	(w => x => x) ((y => y y) (y => y y))
//│ ║        	               ^^^^^^^^^^^^^^^^^^^^^
//│ ╟── ————————— Additional debugging info: —————————
//│ ╟── this constraint:  ‹∀ 0. (α1187_1189' -> α1188_1190')›  <:  α1187_1196    PolymorphicType  TypeVariable
//│ ╙──  ... looks like:  ‹∀ 0. (α1187_1189' -> α1188_1190')›  <:  α1187_1189'
//│ res: 'a -> 'a



// * === With Constrained Types ===

:DontDistributeForalls
:ConstrainedTypes


// * Z combinator:
// :e // Works thanks to inconsistent constrained types...
(f => (x => f (v => (x x) v)) (x => f (v => (x x) v)))
//│ res: ((forall 'a, 'b. ('a -> 'b
//│   where
//│     forall 'c, 'd. ('c -> 'd
//│   where
//│     'e <: (forall 'f, 'g. ('f -> 'g
//│   where
//│     'c <: 'c -> 'f -> 'g)) -> 'd) <: (forall 'c, 'd. ('c -> 'd
//│   where
//│     'e <: (forall 'f, 'g. ('f -> 'g
//│   where
//│     'c <: 'c -> 'f -> 'g)) -> 'd)) -> 'a -> 'b)) -> 'h & 'e) -> 'h

// * Function that takes arbitrarily many arguments:
// :e // Works thanks to inconsistent constrained types...
(f => (x => f (v => (x x) v)) (x => f (v => (x x) v))) (f => x => f)
//│ res: anything -> (forall 'a, 'b. ('a -> 'b
//│   where
//│     forall 'c, 'd. ('c -> 'd
//│   where
//│     forall 'e. 'e -> anything -> 'e <: (forall 'f, 'g. ('f -> 'g
//│   where
//│     'c <: 'c -> 'f -> 'g)) -> 'd) <: (forall 'c, 'd. ('c -> 'd
//│   where
//│     forall 'e. 'e -> anything -> 'e <: (forall 'f, 'g. ('f -> 'g
//│   where
//│     'c <: 'c -> 'f -> 'g)) -> 'd)) -> 'a -> 'b))




:NoCycleCheck

// Exceeds recursion depth limit:
// :e
// (w => x => x) ((y => y y) (y => y y))


