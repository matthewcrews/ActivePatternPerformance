open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers

[<Measure>] type BufferId
[<Measure>] type ConstraintId
[<Measure>] type SplitId
[<Measure>] type MergeId


module DuEncoding =

    [<RequireQualifiedAccess>]
    type Node =
        | BufferId of bufferId : int<BufferId>
        | ConstraintId of constraintId : int<ConstraintId>
        | MergeId of mergeId : int<MergeId>
        | SplitId of splitId : int<SplitId>


module StructDuEncoding =

    [<Struct; RequireQualifiedAccess>]
    type Node =
        | BufferId of bufferId : int<BufferId>
        | ConstraintId of constraintId : int<ConstraintId>
        | MergeId of mergeId : int<MergeId>
        | SplitId of splitId : int<SplitId>

    module Node =

        let ofRefNode (node: DuEncoding.Node) =
            match node with
            | DuEncoding.Node.BufferId bufferId -> Node.BufferId bufferId
            | DuEncoding.Node.ConstraintId constraintId -> Node.ConstraintId constraintId
            | DuEncoding.Node.MergeId mergeId -> Node.MergeId mergeId
            | DuEncoding.Node.SplitId splitId -> Node.SplitId splitId


module IntEncoding =

    [<Struct>]
    type Node =
        {
            Value : int
        }
        static member BufferIdCode = 0
        static member ConstraintIdCode = 1
        static member MergeIdCode = 2
        static member SplitIdCode = 3
        member node.TypeCode = node.Value &&& 0x0000000F
        member node.IdValue = node.Value >>> 4


    module Node =

        let ofRefNode (node: DuEncoding.Node) =
            match node with
            | DuEncoding.Node.BufferId bufferId -> 
                { Value = (int bufferId <<< 4) ^^^ Node.BufferIdCode }
            | DuEncoding.Node.ConstraintId constraintId ->
                { Value = (int constraintId <<< 4) ^^^ Node.ConstraintIdCode }
            | DuEncoding.Node.MergeId mergeId ->
                { Value = (int mergeId <<< 4) ^^^ Node.MergeIdCode }
            | DuEncoding.Node.SplitId splitId ->
                { Value = (int splitId <<< 4) ^^^ Node.SplitIdCode }


    module ActivePattern =

        let inline (|BufferId|ConstraintId|SplitId|MergeId|) (node: Node) =
            // Get the nibble which encodes the type of Id
            let nodeType = node.Value &&& 0x0000000F
            // Get the value of the Id
            let idValue = node.Value >>> 4

            if nodeType = Node.BufferIdCode then
                BufferId (idValue * 1<BufferId>)

            elif nodeType = Node.ConstraintIdCode then
                ConstraintId (idValue * 1<ConstraintId>)

            elif nodeType = Node.MergeIdCode then
                MergeId (idValue * 1<MergeId>)

            elif nodeType = Node.SplitIdCode then
                SplitId (idValue * 1<SplitId>)

            else
                invalidArg (nameof node) "Node Id type does not match known Node Types"


    module PartialActivePattern =

        [<return: Struct>]
        let inline (|BufferId|_|) (node: Node) =
            let nodeType = node.Value &&& 0x0000000F

            if nodeType = Node.BufferIdCode then
                let idValue = node.Value >>> 4
                ValueSome (idValue * 1<BufferId>)
            else
                ValueNone


        [<return: Struct>]
        let inline (|ConstraintId|_|) (node: Node) =
            let nodeType = node.Value &&& 0x0000000F

            if nodeType = Node.ConstraintIdCode then
                let idValue = node.Value >>> 4
                ValueSome (idValue * 1<ConstraintId>)
            else
                ValueNone


        [<return: Struct>]
        let inline (|MergeId|_|) (node: Node) =
            let nodeType = node.Value &&& 0x0000000F

            if nodeType = Node.MergeIdCode then
                let idValue = node.Value >>> 4
                ValueSome (idValue * 1<MergeId>)
            else
                ValueNone


        [<return: Struct>]
        let inline (|SplitId|_|) (node: Node) =
            let nodeType = node.Value &&& 0x0000000F

            if nodeType = Node.SplitIdCode then
                let idValue = node.Value >>> 4
                ValueSome (idValue * 1<SplitId>)
            else
                ValueNone


[<MemoryDiagnoser; HardwareCounters(HardwareCounter.BranchMispredictions, HardwareCounter.CacheMisses)>]
type Benchmarks () =

    let rng = Random 123
    let nodeCount = 100
    let lookupCount = 100
    let loopCount = 100_000

    let nodes =
        [|for i in 0 .. nodeCount - 1 ->
            match rng.Next (0, 4) with
            | 0 -> DuEncoding.Node.BufferId 1<BufferId>
            | 1 -> DuEncoding.Node.ConstraintId 1<ConstraintId>
            | 2 -> DuEncoding.Node.MergeId 1<MergeId>
            | 3 -> DuEncoding.Node.SplitId 1<SplitId>
            | _ -> failwith "The RNG generated a number outside the allowed bounds"
        |]

    let structNodes =
        nodes
        |> Array.map StructDuEncoding.Node.ofRefNode

    let intEncodedNodes =
        nodes
        |> Array.map IntEncoding.Node.ofRefNode


    let randomNodeIndices =
        [|for _ = 0 to loopCount - 1 do
            [|for i in 1 .. lookupCount ->
                rng.Next (0, nodeCount)
            |]
        |]


    [<Benchmark>]
    member _.DuEncodingRandomAccess () =
        let mutable acc = 0

        for lookupsIndex = 0 to loopCount - 1 do
            let lookups = randomNodeIndices[lookupsIndex]

            for lookupIndex = 0 to lookups.Length - 1 do
                let randomNodeIndex = lookups[lookupIndex]

                match nodes[randomNodeIndex] with
                | DuEncoding.Node.BufferId bufferId -> acc <- acc + 1
                | DuEncoding.Node.ConstraintId constraintId -> acc <- acc + 2
                | DuEncoding.Node.MergeId mergeId -> acc <- acc + 3
                | DuEncoding.Node.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.StructDuEncodingRandomAccess () =
        let mutable acc = 0

        for lookupsIndex = 0 to loopCount - 1 do
            let lookups = randomNodeIndices[lookupsIndex]
            
            for lookupIndex = 0 to lookups.Length - 1 do
                let randomNodeIndex = lookups[lookupIndex]

                match structNodes[randomNodeIndex] with
                | StructDuEncoding.Node.BufferId bufferId -> acc <- acc + 1
                | StructDuEncoding.Node.ConstraintId constraintId -> acc <- acc + 2
                | StructDuEncoding.Node.MergeId mergeId -> acc <- acc + 3
                | StructDuEncoding.Node.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.IntEncodingWithActivePatternRandomAccess () =
        let mutable acc = 0


        for lookupsIndex = 0 to loopCount - 1 do
            let lookups = randomNodeIndices[lookupsIndex]
            
            for lookupIndex = 0 to lookups.Length - 1 do
                let randomNodeIndex = lookups[lookupIndex]

                match intEncodedNodes[randomNodeIndex] with
                | IntEncoding.ActivePattern.BufferId bufferId -> acc <- acc + 1
                | IntEncoding.ActivePattern.ConstraintId constraintId -> acc <- acc + 2
                | IntEncoding.ActivePattern.MergeId mergeId -> acc <- acc + 3
                | IntEncoding.ActivePattern.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.IntEncodingWithPartialActivePatternRandomAccess () =
        let mutable acc = 0

        for lookupsIndex = 0 to loopCount - 1 do
            let lookups = randomNodeIndices[lookupsIndex]
            
            for lookupIndex = 0 to lookups.Length - 1 do
                let randomNodeIndex = lookups[lookupIndex]

                match intEncodedNodes[randomNodeIndex] with
                | IntEncoding.PartialActivePattern.BufferId bufferId -> acc <- acc + 1
                | IntEncoding.PartialActivePattern.ConstraintId constraintId -> acc <- acc + 2
                | IntEncoding.PartialActivePattern.MergeId mergeId -> acc <- acc + 3
                | IntEncoding.PartialActivePattern.SplitId splitId -> acc <- acc + 4
                | _ -> failwith "¯\_(ツ)_/¯"

        acc

    [<Benchmark>]
    member _.IntEncodingTypeCodeCheckRandomAccess () =
        let mutable acc = 0

        for lookupsIndex = 0 to loopCount - 1 do
            let lookups = randomNodeIndices[lookupsIndex]
            
            for lookupIndex = 0 to lookups.Length - 1 do
                let randomNodeIndex = lookups[lookupIndex]
                let node = intEncodedNodes[randomNodeIndex]
                let typeCode = node.TypeCode
                let idValue = node.IdValue

                if typeCode = IntEncoding.Node.BufferIdCode then
                    let bufferId = idValue * 1<BufferId>
                    acc <- acc + 1

                elif typeCode = IntEncoding.Node.ConstraintIdCode then
                    let constraintId = idValue * 1<ConstraintId>
                    acc <- acc + 1

                elif typeCode = IntEncoding.Node.MergeIdCode then
                    let mergeId = idValue * 1<MergeId>
                    acc <- acc + 1
                
                elif typeCode = IntEncoding.Node.SplitIdCode then
                    let splitId = idValue * 1<SplitId>
                    acc <- acc + 1

        acc


    [<Benchmark>]
    member _.DuEncodingLinearAccess () =
        let mutable acc = 0

        for _ = 0 to loopCount - 1 do

            for i = 0 to nodes.Length - 1 do

                match nodes[i] with
                | DuEncoding.Node.BufferId bufferId -> acc <- acc + 1
                | DuEncoding.Node.ConstraintId constraintId -> acc <- acc + 2
                | DuEncoding.Node.MergeId mergeId -> acc <- acc + 3
                | DuEncoding.Node.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.StructDuEncodingLinearAccess () =
        let mutable acc = 0

        for _ = 0 to loopCount - 1 do

            for i = 0 to structNodes.Length - 1 do

                match structNodes[i] with
                | StructDuEncoding.Node.BufferId bufferId -> acc <- acc + 1
                | StructDuEncoding.Node.ConstraintId constraintId -> acc <- acc + 2
                | StructDuEncoding.Node.MergeId mergeId -> acc <- acc + 3
                | StructDuEncoding.Node.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.IntEncodingWithActivePatternLinearAccess () =
        let mutable acc = 0


        for _ = 0 to loopCount - 1 do

            for i = 0 to intEncodedNodes.Length - 1 do

                match intEncodedNodes[i] with
                | IntEncoding.ActivePattern.BufferId bufferId -> acc <- acc + 1
                | IntEncoding.ActivePattern.ConstraintId constraintId -> acc <- acc + 2
                | IntEncoding.ActivePattern.MergeId mergeId -> acc <- acc + 3
                | IntEncoding.ActivePattern.SplitId splitId -> acc <- acc + 4

        acc


    [<Benchmark>]
    member _.IntEncodingWithPartialActivePatternLinearAccess () =
        let mutable acc = 0

        for _ = 0 to loopCount - 1 do

            for i = 0 to intEncodedNodes.Length - 1 do

                match intEncodedNodes[i] with
                | IntEncoding.PartialActivePattern.BufferId bufferId -> acc <- acc + 1
                | IntEncoding.PartialActivePattern.ConstraintId constraintId -> acc <- acc + 2
                | IntEncoding.PartialActivePattern.MergeId mergeId -> acc <- acc + 3
                | IntEncoding.PartialActivePattern.SplitId splitId -> acc <- acc + 4
                | _ -> failwith "¯\_(ツ)_/¯"

        acc


    [<Benchmark>]
    member _.IntEncodingTypeCodeLinearAccess () =
        let mutable acc = 0

        for _ = 0 to loopCount - 1 do

            for i = 0 to intEncodedNodes.Length - 1 do

                let node = intEncodedNodes[i]
                let typeCode = node.TypeCode
                let idValue = node.IdValue

                if typeCode = IntEncoding.Node.BufferIdCode then
                    let bufferId = idValue * 1<BufferId>
                    acc <- acc + 1

                elif typeCode = IntEncoding.Node.ConstraintIdCode then
                    let constraintId = idValue * 1<ConstraintId>
                    acc <- acc + 1

                elif typeCode = IntEncoding.Node.MergeIdCode then
                    let mergeId = idValue * 1<MergeId>
                    acc <- acc + 1
                
                elif typeCode = IntEncoding.Node.SplitIdCode then
                    let splitId = idValue * 1<SplitId>
                    acc <- acc + 1

        acc



[<EntryPoint>]
let main _ =

    // I don't care about what Run returns so I'm ignoring it
    let _ = BenchmarkRunner.Run<Benchmarks>()
    0