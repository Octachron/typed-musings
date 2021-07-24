
type stop = Stop
type zero = Zero
type one = One
type ('a,'b) mult = Mult
type 'a neg = Neg

type ('a,'b) eq = Refl: ('a,'a) eq

let cast (type a b) (Refl:(a,b) eq) (x:a) = (x:b)
let (%) (type a b c) (Refl:(a,b) eq) (Refl:(b,c) eq) =  (Refl:(a,c) eq)
let rev (type a b) (Refl: (a,b) eq) = (Refl : (b,a) eq)

module type group_axioms = sig
  type 'a t
  type ('a,'b) eqt = ('a t, 'b t) eq
  val inv: ('a * 'a neg, zero) eqt
  val assoc: ('a * ('b * 'c),  ('a * 'b) * 'c) eqt
  val e: (zero * 'a, 'a) eqt
  val com: ('a * 'b, 'b * 'a) eqt
end

module type eq_monad =
  sig
    type ('a,'b) eqt
    val pure: ('a,'b) eq -> ('a,'b) eqt
    val bind:  (('a,'b) eq -> ('c,'d) eqt) -> ('a,'b) eqt -> ('c,'d) eqt
    val (>>=): ('a,'b) eqt -> (('a,'b) eq -> ('c,'d) eqt) -> ('c,'d) eqt
  end


module Z: sig

  type +'a t

  val zero: zero t
  val one: one t
  val ( + ) : 'a t -> 'b t -> ('a * 'b) t
  val ( - ) : 'a t -> 'b t -> ('a * 'b neg) t

  module Axioms: sig
    include group_axioms with type 'a t := 'a t
    include eq_monad with type ('a,'b) eqt := ('a,'b) eqt
  end
end = struct
  type 'a t = int
  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( - ) = ( - )

   module Axioms = struct
     type ('a,'b) eqt = ('a t, 'b t) eq
     let bind _f _x = Refl
     let pure _ = Refl
     let (>>=) x f = bind f x
     let inv = Refl
     let assoc = Refl
     let e = Refl
     let com = Refl
   end

end

module Units : sig
  type +'a t

  val scalar: float -> zero t

  val ( + ) : 'a t -> 'a t -> 'a t
  val ( - ) : 'a t -> 'a t -> 'a t

  val ( * ) : 'a t -> 'b t -> ('a * 'b) t


  val ( / ) : 'a t -> 'b t -> ('a * 'b neg) t

  module Axioms: sig
    include group_axioms with type 'a t := 'a t
    val pure: ('a,'b) eq -> ('a,'b) eqt
    val bind:  (('a,'b) eq -> ('c,'d) eqt) -> ('a,'b) eqt -> ('c,'d) eqt
    val (>>=): ('a,'b) eqt -> (('a,'b) eq -> ('c,'d) eqt) -> ('c,'d) eqt
  end

  module Make(): sig
    type u
    val one: u t
  end



end = struct

   type 'a t = float
   let scalar x = x
   let ( + )  = ( +.)
   let ( - )  = ( -. )
   let ( * )  = ( *. )
   let ( / ) = ( /. )

   module Axioms = struct
     type ('a,'b) eqt = ('a t, 'b t) eq
     let bind _f _x = Refl
     let pure _ = Refl
     let (>>=) x f = bind f x
     let inv = Refl
     let assoc = Refl
     let e = Refl
     let com = Refl
   end

  module Make() = struct
    type u
    let one = 1.
  end


end


module Meter = Units.Make()
module Second = Units.Make()

let m = Meter.one
let s = Second.one


open Units

module A = Axioms


let ( *. ) s x = cast A.e (scalar s * x)

let e_rev_inv (type a z) (x: (a * a neg, z) eq): (z, a * a neg) eq  =
  let Refl = x in
  Refl


let ( let* ) = A.(>>=)

let (>>|) x f = let open A in
  let* eq = x in
  pure (f eq)

let (>>=) = A.(>>=)

let neg eq =
  let neg0 (type a b) (Refl:(a,b) eq) : (a neg, b neg) A.eqt = Refl in
  eq >>= neg0

let fst eq =
  let fst (type a b c) (Refl:(a,b) eq): (a * c, b * c)  A.eqt = Refl in
  eq >>= fst

let snd eq =
  let snd (type x y a) (Refl:(x,y) eq): (a * x, a * y)  A.eqt = Refl in
  eq >>= snd



let rev_inv () = (A.inv >>| e_rev_inv )

let e_inv_unicity (type a b z) (a: (a * a neg, z) eq) (x:(a * b, z) eq) :  (b, a neg) A.eqt =
  let Refl = x in
  let Refl = e_rev_inv a in
  Refl


let inv_unicity x =
  let* inv = A.inv in
  let* x = x in
  e_inv_unicity inv x

let involution () (type a): (a neg neg, a) A.eqt =
  rev @@ inv_unicity @@ A.com % A.inv

let distrib () (type a b) : (a neg * b neg, (a * b) neg) A.eqt =
  let inv =  A.inv in (* x + -x = 0 *)
  let zpair =  rev (rev inv % rev A.e) in (* 0 + x + -x = 0 *)
  let dpair = fst inv % zpair in  (* (y + -y) + x + -x = 0 *)
  let c = A.assoc % fst A.com % dpair in (* -y + (y + (x + -x) = 0 *)
  let d = snd (rev A.assoc) % c in (* -y + ( (y + x) + -x) = 0 *)
  let e = snd (A.com) % d in (* -y + ( -x + (y + x)) = 0 *)
  let p = rev A.assoc % e in (* (-y + -x +  (y + x) = 0 *)
  let p' = A.com % p in (* (y + x) + -y + -x = 0 *)
  inv_unicity p' (* -y + -x = -(y+x) *)

let sound = 340. *. m / s

let g = 9.81 *. m / (s * s)

let time = sound / g (* (m / s) / (m / (s * s)) *)

let time = cast (snd (rev @@ distrib ())) time (* ((m / s) / m) / (s * s) ^-1 *)
let time = cast (rev A.assoc) time (* ( m  * (( s^-1 / m) / (s * s)^-1) *)
let time = cast (snd A.com) time (* ( m  * m^-1 / (s * s)^-1) * s^-1 *)
let time = cast (snd (rev A.assoc)) time (* m * ( m^-1  / (s * s)^-1) * s^-1 ) *)
let time = cast A.assoc time (* m / m / (s * s)^-1) * s^-1 *)
let time = cast (fst A.inv) time (* zero / (s * s)^-1) s^-1 *)
let time = cast A.e time (* ((s * s)^-1)^1 * s^-1 *)
let time = cast (fst @@ involution ()) time (* s * s / s *)
let time = cast A.(com % assoc) time (* (s^-1 s) * s *)
let time : Second.u t = cast A.(fst (com % inv) % e) time (* s *)
