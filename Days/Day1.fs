module Day1

let input = Helpers.getInput 1

let fuelRequirement mass = mass / 3 - 2

let solve = 
    input |> Seq.sumBy (int >> fuelRequirement)