module AdventOfCode.Intcode

type ParameterMode =
    | Position
    | Immediate

module ParameterMode =
    let parse (parameter: int): ParameterMode =
        if parameter = 0 then Position else Immediate

type Operation =
    | Add
    | Multiply
    | Halt
    | Read
    | Write

type Instruction =
    { Operation: Operation
      Parameters: {| A: ParameterMode
                     B: ParameterMode
                     C: ParameterMode |}
      Length: int }

type Opcode =
    { Code: int
      Operation: Operation
      Length: int }

module Instruction =
    let private opcodes =
        [ { Code = 1
            Operation = Add
            Length = 4 }
          { Code = 2
            Operation = Multiply
            Length = 4 }
          { Code = 3
            Operation = Read
            Length = 2 }
          { Code = 4
            Operation = Write
            Length = 2 }
          { Code = 99
            Operation = Halt
            Length = 1 } ]
        |> List.map (fun opcode -> opcode.Code, opcode)
        |> Map.ofList

    let parse (instruction: int): Instruction =
        let opcode = Map.find (instruction % 100) opcodes
        let parameterModes = instruction / 100
        { Operation = opcode.Operation
          Parameters =
              {| A = ParameterMode.parse (parameterModes &&& 1)
                 B = ParameterMode.parse (parameterModes &&& 10)
                 C = ParameterMode.parse (parameterModes &&& 100) |}
          Length = opcode.Length }

type State =
    { Memory: int []
      mutable Input: int
      mutable Output: int
      IC: int }

type Computer(state: State) =
    member __.Item
        with get (index) = state.Memory.[index]
        and set index value = state.Memory.[index] <- value

    member __.Run() =
        let rec loop ic =
            let instruction = Instruction.parse state.Memory.[ic]
            match instruction.Operation with
            | Halt -> ()
            | Add ->
                if instruction.Parameters.C <> Position
                then failwith "Invalid parameter mode for writing"

                let result =
                    (match instruction.Parameters.A with
                     | Position -> state.Memory.[state.Memory.[ic + 1]]
                     | Immediate -> state.Memory.[ic + 1])
                    + (match instruction.Parameters.B with
                       | Position -> state.Memory.[state.Memory.[ic + 2]]
                       | Immediate -> state.Memory.[ic + 2])

                state.Memory.[state.Memory.[ic + 3]] <- result
                loop (ic + instruction.Length)
            | Multiply ->
                if instruction.Parameters.C <> Position
                then failwith "Invalid parameter mode for writing"

                let result =
                    (match instruction.Parameters.A with
                     | Position -> state.Memory.[state.Memory.[ic + 1]]
                     | Immediate -> state.Memory.[ic + 1])
                    * (match instruction.Parameters.B with
                       | Position -> state.Memory.[state.Memory.[ic + 2]]
                       | Immediate -> state.Memory.[ic + 2])

                state.Memory.[state.Memory.[ic + 3]] <- result
                loop (ic + instruction.Length)
            | Read ->
                if instruction.Parameters.A <> Position
                then failwith "Invalid parameter mode for writing"
                loop (ic + instruction.Length)
            | Write ->
                if instruction.Parameters.A <> Position
                then failwith "Invalid parameter mode for writing"
                loop (ic + instruction.Length)

        loop state.IC
