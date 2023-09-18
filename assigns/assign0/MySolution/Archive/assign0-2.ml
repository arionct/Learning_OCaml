#use "./../assign0.ml";;

let isPrime n0 =
  let rec is_prime_helper i =
    if i * i > n0 then true
    else if n0 mod i = 0 then false
    else is_prime_helper (i + 1)
  in
  n0 > 1 && is_prime_helper 2
;;