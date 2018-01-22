let memoize (f : 'a -> 'b) =
    let lookup = System.Collections.Concurrent.ConcurrentDictionary<'a,'b>()
    fun n -> lookup.GetOrAdd(n,f)