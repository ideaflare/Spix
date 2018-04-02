type Big = System.Numerics.BigInteger

let rec gcd a b =
    if b = 0I then a
    else gcd b (a % b)

[<CustomEquality;CustomComparison>]
type Rational =
    { Numerator : Big; Denominator : Big }
    member me.Simplify =
        let c' = gcd me.Numerator me.Denominator
        { Numerator = me.Numerator / c'; Denominator = me.Denominator / c' }
    static member Compare (r1, r2) =
        let n1' = r1.Numerator * r2.Denominator
        let n2' = r2.Numerator * r1.Denominator
        Big.Compare(n1',n2')
    static member Equal (r1, r2) = Rational.Compare(r1,r2) = 0
    override r1.Equals(obj) = 
        match obj with
        | :? Rational as r2 -> Rational.Equal(r1,r2)
        | _ -> false
    override me.GetHashCode() = 17 * me.Numerator.GetHashCode() * me.Denominator.GetHashCode()
    override me.ToString() = sprintf "%A/%A" me.Numerator me.Denominator
    interface System.IEquatable<Rational> with
        member r1.Equals(r2) = Rational.Equal(r1,r2)
    interface System.IComparable<Rational> with
        member r1.CompareTo(r2) = Rational.Compare(r1,r2)
    interface System.IComparable with
        member r1.CompareTo(obj) =
            match obj with
            | :? Rational as r2 -> Rational.Compare(r1,r2)
            | _ -> 1

let (/.) n d = { Numerator = n; Denominator = d }

let add r1 r2 = { Numerator = r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator;
                  Denominator = r1.Denominator * r2.Denominator }

let inverse rational = { Numerator = rational.Denominator; Denominator = rational.Numerator }