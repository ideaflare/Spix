let factorial n =
    let rec f' n product =
        match n with
        | fin when fin < 2I -> product
        | _ -> f' (n - 1I) (product * n)
    f' n 1I