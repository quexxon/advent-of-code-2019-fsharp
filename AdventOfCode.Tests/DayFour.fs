module AdventOfCode.Tests.DayFour

open AdventOfCode.DayFour
open Expecto

[<Tests>]
let tests =
    testList
        "Advent of Code - Day Four"
        [ test "Pad digits works for 0" {
              Expect.equal (padDigits 0 0 0) 0 "Padding 0 with 0 repetitions of 0 should be 0" } ]
