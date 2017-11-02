let sequence =
    let rec fib a b = seq {
        yield a
        yield! fib b (a + b)
    }
    fib 0 1