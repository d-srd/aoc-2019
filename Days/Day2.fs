module Day2

let input = [1;9;10;3;2;3;11;0;99;30;40;50]

let opcode code currentIndex data = 
    let value position = position |> List.item
    match code with
    | 1 ->
        let lhs = currentIndex + 1 |> List.item
        let rhs = currentIndex + 2 |> List.item
        let dst = currentIndex + 3 |> List.item
        Some {| Source = dst data; Value = value (lhs data) data + value (rhs data) data |}
    | 2 -> 
        let lhs = currentIndex + 1 |> List.item
        let rhs = currentIndex + 2 |> List.item
        let dst = currentIndex + 3 |> List.item
        Some {| Source = dst data; Value = value (lhs data) data * value (rhs data) data |}
    | 99 -> 
        None
    | _ -> 
        None

let updateElementIfNeeded index element (value: {| Source: int; Value: int |}) =
    if index = value.Source then
        value.Value
    else
        element

let rec parse data index =
    let code = List.item index data
    let result = opcode code index data
    match result with
    | None ->
        data
    | Some value ->
        let updated = data |> List.mapi (fun i p -> updateElementIfNeeded i p value)
        parse updated (index + 4)

let solve = parse input 0