module Day2

let input = [1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;6;19;1;9;19;23;2;23;10;27;1;27;5;31;1;31;6;35;1;6;35;39;2;39;13;43;1;9;43;47;2;9;47;51;1;51;6;55;2;55;10;59;1;59;5;63;2;10;63;67;2;9;67;71;1;71;5;75;2;10;75;79;1;79;6;83;2;10;83;87;1;5;87;91;2;9;91;95;1;95;5;99;1;99;2;103;1;103;13;0;99;2;14;0;0]

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

let replaceInput index element x y =
    if index = 1 then
        x
    else if index = 2 then
        y
    else 
        element

let replaceInputs x y data = data |> List.mapi (fun i p -> replaceInput i p x y)

let matchesResult pair input = 
    let (i, j) = pair
    let currentInput = replaceInputs i j input
    let result = parse currentInput 0 |> List.item 0
    if result = 19690720 then
        Some {| X = i; Y = j|}
    else
        None

let findSolution data =
    let inputs = seq {
        for i in 0 .. 100 do
            for j in 0 .. 100 do
                yield (i, j)
    }
    inputs
    |> Seq.map (fun p -> matchesResult p data)
    |> Seq.find Option.isSome
    |> Option.get

let result = findSolution input
