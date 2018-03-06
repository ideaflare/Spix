type Big = System.Numerics.BigInteger

type Rational = { Numerator : Big; Denominator : Big }

let (/.) n d = { Numerator = n; Denominator = d }

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let inverse rational = { Numerator = rational.Denominator; Denominator = rational.Numerator }

let print (r:Rational) = sprintf "%A/%A" r.Numerator r.Denominator