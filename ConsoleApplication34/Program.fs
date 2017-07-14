// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
let greatestNumber (a:int) (b:int) (c:int) : string =
    if a > b && a > c then
        "a is greatest"
    else if b > a && b > c then
        "b is greatest" 
    else
        "c is greatest" 
         
let tau0 (a:Option<'a>) : List<'a> =
    match a with
    | Some(a) -> [a]
    | None -> []

let plus (a:Option<'a>) : Option<'b> =
    match a with
    | Some(a) -> Some(a + 1)
    | None -> None  


let mapFuncTest (x: Option<'a>) : Option<'a> =
    match x with
    | Some(x) -> Some(x + 1)
    | None -> None

let rec MapList (func:(Option<'a> -> Option<'a>)) (list:List<'a>) : List<'a> =
    match list with
    | [] -> []
    | h::t -> 
        match func (Some(h)) with
        | Some(h) -> h::MapList func t
        | None -> MapList func t
         
type expression =
    | Number of int
    | Sum of expression * expression
    | Divide of expression * expression
    | Plus of expression * expression
    
let rec evaluate (expr:expression) =
    match expr with
    |   Number(n) -> n
    |   Sum(first, second) -> evaluate first + evaluate second


let plusValue (a:Option<'a>) : Option<'b> =
    match a with
    | Some(a) -> Some(a + 5)
    | None -> None


let rec zip (list1:List<'a>) (list2:List<'b>) =
    match list1, list2 with
    | [], [] -> []
    | h1::t1, h2::t2 -> (h1, h2)::zip t1 t2 

type DoB = {year:int; month:int; day:int}
type Person = {name:string; date:DoB}

let increaseDate (person:Person) =
    match person.date.year, person.date.month, person.date.day with
    | Person -> person.date.year + 1
    

type Ship = {
    position:(float * float);
    projectiles:List<(float * float)>
}

let barld = {name="Pieter";date={year=8;month=8;day=1994}}

type Datee = {
  Day:int;
  Month:int;
  Year:int;
}

type Persone = {
  Name:string;
  Surname:string;
  DOB:Datee;
}


let getOlder person:Persone = {person with DOB ={Day = person.DOB.Day + 1; Month = person.DOB.Month + 1; Year = person.DOB.Year + 1}}

let lol = {Name="Barld";Surname="Boot";DOB={Day=8;Month=8;Year=1994}}

[<EntryPoint>]
let main argv = 
    printfn "%A" (greatestNumber 1 4 2)
    printfn "%A" (increaseDate barld)
    printfn "%A" (getOlder lol)
    printfn "%A" (evaluate (Sum(Number(4), Number(3))))
    0 // return an integer exit code
