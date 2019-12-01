module Day1

let input = Helpers.getInput 1

let rec fuelRequirement mass = 
    let initialRequirement = mass / 3 - 2
    if initialRequirement > 0 then 
        let additionalFuel = fuelRequirement initialRequirement
        if additionalFuel > 0 then
            initialRequirement + additionalFuel
        else
            initialRequirement
    else
        initialRequirement

let solve = input |> Seq.sumBy (int >> fuelRequirement)