let firstOfMonthSundays years =
    let isSunday year month day =
        let dt = System.DateTime(year,month,day)
        dt.DayOfWeek = System.DayOfWeek.Sunday
    let firstSundaysMonths year =
        [1..12]
        |> List.filter (fun month -> isSunday year month 1)
        |> List.length
    years
    |> List.sumBy firstSundaysMonths

printfn "real = %A" (firstOfMonthSundays [1901..2000])