module ICPC
open System

let fs (s:string) =
  let rec f s c w l =
    match String.length s with
      | 0 -> l @ [w] @ ["."]
      | _ ->
        match c with
          | '.' | ',' -> (string c + " ", l @ [w]) ||> f s.[2..] s.[1] 
          | ' ' -> f s.[1..] s.[0] " " (l @ [w])
          | c -> f s.[1..] s.[0] (w + string c) l 
  f s.[1..] s.[0] "" []

let bs (s:string) =
  let rec b s c w l =
    match String.length s with
      | 0 -> l @ [w + string c]
      | _ ->
        match c with
          | ' ' -> b s.[1..] s.[0] "" (l @ [w])
          | c -> b s.[1..] s.[0] (w + string c) l 
  b s.[1..] s.[0] "" []

let bc (s:string) =
  s.[0] = ','

let ec (s:string) =
  s.[String.length s |> (+) -1] = ','

let wb (s1:string) (s2:string) =
  match s1.[1..] = s2 with
    | true ->  s1
    | false -> s2

let we (s1:string) (s2:string) =
  match s1.[..String.length s1 - 2] = s2 with
    | true ->  s1
    | false -> s2

let cc (s:string) =
  let rec cc s c =
    match String.length s with
      | 0 -> 
        match c with
          | '.' -> true
          | _ -> false
      | _ ->
        match c with
          | ' ' ->
            match 'a' <= s.[0] && s.[0] <= 'z'with
              | true -> cc s.[1..] s.[0]  
              | false -> false
          | '.' | ',' ->
            match s.[0] = ' ' with
              | true -> cc s.[1..] s.[0]
              | false -> false
          | c ->
            match 'a' <= c && c <= 'z' with
              | true -> cc s.[1..] s.[0]
              | false -> false
  cc s ' '

let vs s =
  (String.length s >= 2) && cc s

let commaSprinkler (input:string) =
  let rec cs i =
    let w = (bs i, bs i |> List.filter ec) ||> List.fold (fun w t -> List.map (we t) w) |> String.concat " " |> fs
    let l = w |> List.filter bc
    let w = List.fold (fun w l -> List.map (wb l) w) w l |> String.concat ""
    match i = w with
     | true -> Some(i)
     | false -> cs w 
  match vs input with
    | true -> cs input
    | false -> None

type RivRec =
  {
    rivers : (int * int) list
    max : int
  }

let lif (s:string) =
  let rec lf s c n l cl =
    match String.length s with
      | 0 -> l::cl
      | _ ->
        match c with
          | ' ' -> lf s.[1..] s.[0] (n + 1) ((n, 1)::l) cl
          | '\n' -> lf s.[1..] s.[0] 0 [] (l::cl)
          | _ -> lf s.[1..] s.[0] (n + 1) l cl
  lf s.[1..] s.[0] 0 [] []

let sp (p1,_) (p2, _) =
  p1 - 1 <= p2 && p2 <= p1 + 1

let rj (_, l1) (p2, _) =
  (p2, l1 + 1)

let pm (p1, _) (p2, _) =
  p1 <> p2
  
let nr {rivers = r; max = m} l =
  let rec tr r q nm u =
    match r with
      | [] -> {rivers = q @ u; max = nm}
      | ((_, rl) as e)::f ->
        match List.filter (sp e) l with
          | [] ->
            let (_, l) = e
            match rl > nm with
              | true -> tr f q rl u
              | false -> tr f q nm u
          | m -> tr f (List.map (rj e) m @ q) nm (List.filter (pm e) u)
  tr r [] m l

let lr s =
  let e::r = lif s 
  let {max = m} = List.fold nr {rivers = e; max = 0} r
  m

let rfs (s:string) =
  let rec f s c w l =
    match String.length s with
      | 0 -> l @ [w + string c]
      | _ ->
        match c with
          | ' ' -> f s.[1..] s.[0] " " (l @ [w])
          | c -> f s.[1..] s.[0] (w + string c) l 
  f s.[1..] s.[0] "" []

let rsm (s:string) =
  let rec f (s:string) c wl ml wc =
    match c with
      | "" -> true, ml, (wc + 1)
      | c ->
        match c with
          | " " -> 
            match wl = 0 || s = "" with
              | true -> false, 0, 0
              | false ->
                match wl > ml with
                  | true -> f s.[1..] s.[0..0] 0 wl (wc + 1)
                  | false -> f s.[1..] s.[0..0] 0 ml (wc + 1)
          | c -> 
            match ("a" <= c && c <= "z") || ("A" <= c && c <= "Z") with
              | true -> f s.[1..] s.[0..0] (wl + 1) ml wc 
              | false -> false, 0, 0
  f s.[1..] s.[0..0] 0 0 0

let ss s n =
  let s = rfs s
  let rec ms w l o =
    match w with
      | [] -> o + ("\n" + l)
      | e::r ->
        match String.length (l + e) > n with
          | true -> ms r (string e).[1..] (o + "\n" + l)
          | false -> ms r (l + e) o
  ms s.[1..] s.[0] ""

let rivers input = // I am sure there is a way to do this in very good time, but I am lazy
  let v, ml, wc = rsm input
  match v, ml <= 80, wc >= 2 with
    | true, true, true ->
      let rec fl w mw mr =
        match w = String.length input + 1 with
          | true -> Some(mw, mr)
          | false ->
            let rl = ss input w |> lr
            match rl > mr with
              | true -> fl (w + 1) w rl
              | false -> fl (w + 1) mw mr
      fl ml ml 0
    | _ -> None

[<EntryPoint>]
let main argv =
  ss "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country" 14 |> lr |> printfn "%d"
  0 // return an integer exit code
