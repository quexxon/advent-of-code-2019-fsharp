module AdventOfCode.DayTwo

open System.IO

type Instruction =
    | Add
    | Multiply
    | Halt

module Instruction =
    let private opcodes =
        Map.ofList [ (1, Add)
                     (2, Multiply)
                     (99, Halt) ]

    let parse (opcode: int): Instruction = Map.find opcode opcodes

type Computer(ram: int [], ?pc: int) =
    let pc = defaultArg pc 0

    member _.Item
        with get (index) = ram.[index]
        and set index value = ram.[index] <- value

    member this.Run() =
        let rec loop pc =
            match Instruction.parse ram.[pc] with
            | Halt -> ()
            | Add ->
                ram.[ram.[pc + 3]] <- ram.[ram.[pc + 1]] + ram.[ram.[pc + 2]]
                loop (pc + 4)
            | Multiply ->
                ram.[ram.[pc + 3]] <- ram.[ram.[pc + 1]] * ram.[ram.[pc + 2]]
                loop (pc + 4)

        loop pc

type Solution() as self =
    inherit Util.Solution<int>("Day Two", "02.txt")

    let memory =
        File.ReadAllText(self.InputPath).Split(',')
        |> Array.map int

    member private __.Run(noun: int, verb: int) =
        let computer = Computer(Array.copy memory)
        computer.[1] <- noun
        computer.[2] <- verb
        computer.Run()
        computer.[0]

    override this.Part1() = this.Run(12, 2)

    override this.Part2() =
        let target = 19690720

        let rec loop noun verb =
            match this.Run(noun, verb) with
            | result when result = target -> 100 * noun + verb
            | _ when noun > 99 -> -1
            | _ when verb > 99 -> loop (noun + 1) 0
            | _ -> loop noun (verb + 1)

        loop 0 0
