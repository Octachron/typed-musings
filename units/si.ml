
type z = Zero
type 'a succ = S

type 'a zero = 'a * 'a

type 'a one = 'a * 'a succ
type 'a two = 'a * 'a succ succ
type 'a mone = 'a succ * 'a

module Unit: sig
  type +'a t

  (* Generators *)
  val scalar: float -> <m:_ zero; s: _ zero; kg: _ zero> t
  val float: <m:_ zero; s: _ zero; kg: _ zero> t -> float
  val m: <m:_ one; s:_ zero; kg:_ zero> t
  val s: <m:_ zero; s:_ one; kg:_ zero> t
  val kg: <m:_ zero; s:_ zero; kg:_ one> t

  (* Arithmetic operations *)
  val ( + ): 'a t -> 'a t -> 'a t
  val ( * ):
    <m:'m_low * 'm_mid; s:'s_low * 's_mid; kg:'kg_low * 'kg_mid>  t ->
    <m:'m_mid * 'm_high; s:'s_mid * 's_high; kg:'kg_mid * 'kg_high>  t ->
    <m:'m_low * 'm_high; s:'s_low * 's_high; kg:'kg_low * 'kg_high>  t

  val ( / ) :
    <m:'m_low * 'm_high; s:'s_low * 's_high; kg:'kg_low * 'kg_high>  t ->
    <m:'m_mid * 'm_high; s:'s_mid * 's_high; kg:'kg_mid * 'kg_high>  t ->
    <m:'m_low * 'm_mid; s:'s_low * 's_mid; kg:'kg_low * 'kg_mid> t

  (* normalization *)
  val unshift_m : <m: ('a succ -> 'b succ) as 'm; s:'s; kg:'kg> t -> <m: 'm; s:'s; kg:'kg> t
  val unshift_s : <m: 'm; s:('a succ -> 'b succ) as 's; kg:'kg> t -> <m: 'm; s:'s; kg:'kg> t
  val unshift_kg : <m:'m; s:'s; kg: ('a succ -> 'b succ) as 'kg> t -> <m: 'm; s:'s; kg:'kg> t

end = struct
  type +'a t = float

   let scalar x = x
   let float x = x
   let ( + ) = ( +. )

  let ( * ) = ( *. )
  let ( / ) = ( /. )

  let m = 1.
  let s = 1.
  let kg = 1.

  let unshift_m x = x
  let unshift_s x = x
  let unshift_kg x = x

end

open Unit

let ( *. ) x y = scalar x * y

let c : <m:_ one; s: _ mone; kg:_ zero> t = 299_792_458. *. m / s

let t = m / m + s/s

let ua = 149_597_870_700. *. m

let year = 86400. *. (365. *. s)

let test = float @@ (c * year) / ua


let what x y = m * x +  ((y/m) * m) * m

(*
let what x = m * x +  ((x/m) * m) * m
let wrong = year + ua
*)
