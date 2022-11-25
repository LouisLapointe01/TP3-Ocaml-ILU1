let rec premierCh n = 
  if n < 0 then failwith "error"
  else 
  if n >= 10 then premierCh(toutSaufDer n)
  else n
    
let rec toutSaufPrem n = 
  if n < 0 then failwith "error" 
  else     
  if n < 10 then 0
  else if n < 100 then dernierCh(n)
  else dernierCh(n) + 10 * (toutSaufPrem(toutSaufDer(n)));;

let rec estPalindrome n = abs(n)<10 || (premierCh(abs(n))=dernierCh(abs(n)) && estPalindrome(toutSaufDer(abs(n)))=estPalindrome(toutSaufPrem(abs(n))))

let rec nbOccs c n = 
  let der = if dernierCh n = c then 1 else 0 in
  if n < 10 then der else der+nbOccs c (toutSaufDer(n));; 

let rec iterer  n f x = 
  if n = 0 then x 
  else iterer(n-1) f(f(x));;

let id a = a;;

let compose = fun f q x -> f(q x );;

let rec iterer2 n f = 
  if n = 0 then id 
  else compose f(iterer2 (n-1) f);;


let rec itererBis f p x = if p x then x else itererBis f p (f x);;
(*faire qq fp acks*)
    
let rec qqsoit n p = n <= 0 || p n && (qqsoit (n-1) p);;
  
let rec fastpow n e =
  if e = 1 then n
  else let res = fastpow ( n * n ) (e asr 1) in if e land 1 = 1 then res * n else res
     
let fastpow n  e =
  if e = 0 then 1
  else if e < 0 then failwith "fastpow"
  else fastpow n e

let rec ack (m,n)= if m<0 || n<0 then failwith("erreur") else
  if m=0 then n+1 else
  if m>0 && n=0 then ack ((m-1),1) else 
    ack ((m-1),ack (m,(n-1)));;

