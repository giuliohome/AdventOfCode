module MyQueue   // https://stackoverflow.com/a/33483859

    type queue<'a> =
        | Queue of 'a list * 'a list

    let empty = Queue([], [])

    let enqueue q e = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)

    let length q =
        match q with
        | Queue(fs, bs) -> fs.Length + bs.Length

    let dequeue (q:byref<queue<'a>>) = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> 
            q <- Queue(fs, bs)
            b
        | Queue(fs, []) -> 
            let bs = List.rev fs
            q <- Queue([], bs.Tail)
            bs.Head

