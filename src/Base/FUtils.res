let fold_left: (('a, 'b) => 'b, 'a, list<'b>) => 'a = (f, init, ls) => {
    let rec fold_left_internal = (lls, acc) => {
        switch lls {
        | list{} => acc
        | list{x, ...xs} => fold_left_internal(xs, f(acc, x))
        }
    }

    fold_left_internal(ls, init)
}

let reverse: list<'a> => list<'a> = ls => {
    let rec rev_internal = (acc, lls) =>
        switch lls {
        | list{} => acc
        | list{x, ...xs} => rev_internal(list{x, ...acc}, xs)
        }

    ls |> rev_internal(list{})
}
