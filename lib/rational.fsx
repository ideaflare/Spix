type Big = System.Numerics.BigInteger

type Rational = { Numerator : Big; Denominator : Big }

let rec gcd a b =
    if b = 0I then a
    else gcd b (a % b)

let simplify (r:Rational) =
    let c' = gcd r.Numerator r.Denominator
    { Numerator = r.Numerator / c'; Denominator = r.Denominator / c' }

let (/.) n d = { Numerator = n; Denominator = d }

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let inverse rational = { Numerator = rational.Denominator; Denominator = rational.Numerator }

let compare r1 r2 =
    let n1' = r1.Numerator * r2.Denominator
    let n2' = r2.Numerator * r1.Denominator
    Big.Compare(n1',n2')

let print = simplify >> (fun r -> sprintf "%A/%A" r.Numerator r.Denominator)