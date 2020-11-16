module AdventOfCode.DayFour

let inline (/%) x y = (x / y, x % y)

let rec padDigits start value places =
    if places > 10
    then invalidArg "places" "A value greater than 10 for places will overflow Int32 in all cases"

    let result =
        if places <= 0
        then start
        else padDigits (start * 10 + value) value (places - 1)

    if result < start then failwith "Overflow" else result

type Solution() =
    inherit Util.Solution<int>("Day Four", "04.txt")

    let input = {| Start = 124075; Stop = 580769 |}

    let findMatchingPasswords doubleFix start stop =
        let rec loop (matches: int list) candidate work prevDigit (doubles: Set<int>) place =
            if candidate > stop then
                matches
            else
                match work with
                | (0, 0) ->
                    let matches' =
                        if doubles.IsEmpty then matches else candidate :: matches

                    let candidate' = candidate + 1
                    loop matches' candidate' (candidate' /% 10) 10 Set.empty 1
                | (digits, digit) when digit = prevDigit ->
                    let doubles' =
                        if doubleFix then
                            if doubles.Contains(digit) then
                                doubles.Remove(digit)
                            else
                                doubles.Add(digit)
                        else
                            doubles.Add(digit)

                    loop matches candidate (digits /% 10) digit doubles' (place + 1)
                | (digits, digit) when digit <= prevDigit ->
                    loop matches candidate (digits /% 10) digit doubles (place + 1)
                | (digits, digit) ->
                    let candidate' = padDigits digits digit place
                    loop matches candidate' (candidate' /% 10) 10 Set.empty 1

        loop [] start (start /% 10) 10 Set.empty 1

    override __.Part1() =
        let matches =
            findMatchingPasswords false input.Start input.Stop

        matches.Length

    override __.Part2() =
        let matches =
            findMatchingPasswords true input.Start input.Stop

        matches.Length
