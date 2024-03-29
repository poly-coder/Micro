﻿[<AutoOpen>]
module Micro.Preamble

let konst k = fun _ -> k
let flip fn = fun a b -> fn b a

let curry f = fun x y -> f(x, y)
let uncurry f = fun (x, y) -> f x y

let curry2 f = fun x y z -> f(x, y, z)
let uncurry2 f = fun (x, y, z) -> f x y z
