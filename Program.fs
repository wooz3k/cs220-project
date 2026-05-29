open System
open System.Diagnostics
open System.IO
open System.Threading

type Operation =
    | Add of int
    | Subtract of int
    | Multiply of int
    | Divide of int

type Gate =
    { Label: string
      Operation: Operation }

type Monster =
    { Name: string
      Strength: int
      IsFinalBoss: bool }

type Stage =
    { Number: int
      LeftGate: Gate
      RightGate: Gate
      Monster: Monster option }

type Lane =
    | LeftLane
    | RightLane

type StageResult =
    | Continue of int * bool * string list
    | Finished of bool * int * string list

let applyOperation soldiers op =
    match op with
    | Add v -> soldiers + v
    | Subtract v -> soldiers - v
    | Multiply v -> soldiers * v
    | Divide v -> soldiers / v

let describeOperation op =
    match op with
    | Add v -> sprintf "+%d" v
    | Subtract v -> sprintf "-%d" v
    | Multiply v -> sprintf "x%d" v
    | Divide v -> sprintf "/%d" v

let random = Random()
let totalStages = 15

let clamp low high value =
    value |> max low |> min high

let resultGapLimit stageNumber soldiers =
    max (2 + stageNumber / 2) (soldiers / 8)

let operandGapLimit stageNumber resultGap =
    max 1 (min resultGap (3 + stageNumber / 3))

let randomNonZeroDelta limit =
    let magnitude = random.Next(1, max 2 (limit + 1))
    if random.Next(2) = 0 then -magnitude else magnitude

let flipPair left right =
    if random.Next(2) = 0 then left, right else right, left

let randomCloseAddPair stageNumber soldiers =
    let resultGap = resultGapLimit stageNumber soldiers
    let operandGap = operandGapLimit stageNumber resultGap
    let baseValue = random.Next(3 + stageNumber, 8 + stageNumber * 2)
    let otherValue = baseValue + random.Next(1, operandGap + 1)
    flipPair (Add baseValue) (Add otherValue)

let randomBalancedAddMultiplyPair stageNumber soldiers =
    let resultGap = resultGapLimit stageNumber soldiers

    let multiplier =
        if soldiers < 30 && random.Next(3) = 0 then 3 else 2

    let addValue =
        max 1 (soldiers * (multiplier - 1) + randomNonZeroDelta resultGap)

    flipPair (Add addValue) (Multiply multiplier)

let randomCloseSubtractPair stageNumber soldiers =
    if soldiers <= 4 then
        randomCloseAddPair stageNumber soldiers
    else
        let resultGap = resultGapLimit stageNumber soldiers
        let operandGap = operandGapLimit stageNumber resultGap
        let maxSub = max 2 (min (soldiers - 1) (5 + stageNumber * 2))
        let baseValue = random.Next(1, maxSub)
        let maxDelta = max 1 (min operandGap (maxSub - baseValue))
        let otherValue = baseValue + random.Next(1, maxDelta + 1)
        flipPair (Subtract baseValue) (Subtract otherValue)

let randomBalancedSubtractDividePair stageNumber soldiers =
    if soldiers <= 8 then
        randomCloseAddPair stageNumber soldiers
    else
        let resultGap = resultGapLimit stageNumber soldiers
        let divisor = random.Next(2, 4)
        let targetSubtraction = soldiers - (soldiers / divisor)
        let subtractValue = clamp 1 (soldiers - 1) (targetSubtraction + randomNonZeroDelta resultGap)
        flipPair (Subtract subtractValue) (Divide divisor)

let randomSmallAddSubtractPair stageNumber soldiers =
    let resultGap = resultGapLimit stageNumber soldiers
    let budget = max 2 (min resultGap (4 + stageNumber / 2))
    let addValue = random.Next(1, budget)
    let subtractValue = random.Next(1, budget - addValue + 1)
    flipPair (Add addValue) (Subtract subtractValue)

let makeMonster name strength isFinalBoss =
    { Name = name
      Strength = strength
      IsFinalBoss = isFinalBoss }

let rec randomGatePair stageNumber soldiers =
    let leftOperation, rightOperation =
        match random.Next(5) with
        | 0 -> randomCloseAddPair stageNumber soldiers
        | 1 -> randomBalancedAddMultiplyPair stageNumber soldiers
        | 2 -> randomCloseSubtractPair stageNumber soldiers
        | 3 -> randomBalancedSubtractDividePair stageNumber soldiers
        | _ -> randomSmallAddSubtractPair stageNumber soldiers

    let leftResult = applyOperation soldiers leftOperation
    let rightResult = applyOperation soldiers rightOperation
    let bestResult = max leftResult rightResult
    let resultGap = abs (leftResult - rightResult)
    let maxGap = resultGapLimit stageNumber soldiers

    if leftResult > 0 && rightResult > 0 && leftResult <> rightResult && bestResult > 5 && resultGap <= maxGap then
        leftOperation, rightOperation
    else
        randomGatePair stageNumber soldiers

let monsterTemplate stageNumber =
    match stageNumber with
    | 3 -> Some("Goblin Horde", false)
    | 6 -> Some("Stone Golem", false)
    | 9 -> Some("Arcane Swarm", false)
    | 12 -> Some("Iron Colossus", false)
    | 15 -> Some("Dragon Lord", true)
    | _ -> None

let monsterStrength bestResult isFinalBoss =
    let divisor = if isFinalBoss then 2 else 3
    max 1 (min (bestResult - 1) (bestResult / divisor))

let makeStage number soldiers =
    let leftOperation, rightOperation = randomGatePair number soldiers
    let leftResult = applyOperation soldiers leftOperation
    let rightResult = applyOperation soldiers rightOperation
    let bestResult = max leftResult rightResult

    let monster =
        monsterTemplate number
        |> Option.map (fun (name, isFinalBoss) ->
            makeMonster name (monsterStrength bestResult isFinalBoss) isFinalBoss)

    let nextSoldiers =
        match monster with
        | Some value -> bestResult - value.Strength
        | None -> bestResult

    { Number = number
      LeftGate = { Label = "L"; Operation = leftOperation }
      RightGate = { Label = "R"; Operation = rightOperation }
      Monster = monster },
    max 1 nextSoldiers

let rec buildStages number soldiers acc =
    if number > totalStages then
        List.rev acc
    else
        let stage, nextSoldiers = makeStage number soldiers
        buildStages (number + 1) nextSoldiers (stage :: acc)

let newGameStages () = buildStages 1 10 []

let mutable stages = newGameStages ()

let useAnsi = not Console.IsOutputRedirected

let paint code text =
    if useAnsi then sprintf "\u001b[%sm%s\u001b[0m" code text else text

let cyan t = paint "36;1" t
let green t = paint "32;1" t
let red t = paint "31;1" t
let yellow t = paint "33;1" t
let dim t = paint "2" t
let gold t = paint "38;5;220;1" t
let orange t = paint "38;5;214;1" t
let blueBg t = paint "44;97" t
let laneBg t = paint "48;5;238;38;5;250" t

let clearScreen () =
    if not Console.IsOutputRedirected then
        try Console.Clear() with _ -> ()

let laneW = 23
let halfBorder = laneW + 2
let wideW = laneW * 2 + 3

let fit w (s: string) =
    if s.Length <= w then s else s.Substring(0, w)

let center w text =
    let c = fit w text
    let l = (w - c.Length) / 2
    let r = w - c.Length - l
    String.replicate l " " + c + String.replicate r " "

let borderHalf = String.replicate halfBorder "\u2550"

let printRoadTop () =
    printfn "    %s" (cyan (sprintf "\u2554%s\u2566%s\u2557" borderHalf borderHalf))

let printRoadBottom () =
    printfn "    %s" (cyan (sprintf "\u255A%s\u2569%s\u255D" borderHalf borderHalf))

let printLaneLine leftText (styleL: string -> string) rightText (styleR: string -> string) =
    let l = styleL (center laneW leftText)
    let r = styleR (center laneW rightText)
    printfn "    %s %s %s %s %s" (cyan "\u2551") l (cyan "\u2502") r (cyan "\u2551")

let printWideLine text (style: string -> string) =
    let c = style (center wideW text)
    printfn "    %s %s %s" (cyan "\u2551") c (cyan "\u2551")

let printBlankLane frame row =
    let markL = if (row + frame) % 5 = 0 then "\u00b7" else ""
    let markR = if (row + frame + 2) % 5 = 0 then "\u00b7" else ""
    printLaneLine markL laneBg markR laneBg

let soldierUnits soldiers =
    max 1 (min 16 ((soldiers + 4) / 5))

let armyRows soldiers frame =
    let units = soldierUnits soldiers
    let top = max 1 (units / 2)
    let lean = if frame % 2 = 0 then " " else "  "

    [ "  ARMY"
      lean + String.replicate top "O "
      String.replicate units "O "
      " " + String.replicate (min units 14) "O " ]

let monsterArt monster =
    if monster.IsFinalBoss then
        [ sprintf "=== %s ===" monster.Name
          sprintf "Strength: %d" monster.Strength
          "        /\\    /\\"
          "       /  \\  /  \\"
          "      ( @    @ )"
          "       \\  \\/  /"
          "     /||======||\\"
          "    | ||      || |"
          "      ||      ||" ]
    else
        match monster.Name with
        | "Goblin Horde" ->
            [ sprintf "=== %s ===" monster.Name
              sprintf "Strength: %d" monster.Strength
              "      /\\ /\\ /\\"
              "     (oo)(oo)(oo)"
              "     /||\\/||\\/||\\" ]
        | "Stone Golem" ->
            [ sprintf "=== %s ===" monster.Name
              sprintf "Strength: %d" monster.Strength
              "       [###]"
              "      /|   |\\"
              "     | |   | |"
              "     |_|___|_|"
              "       || ||" ]
        | _ ->
            [ sprintf "=== %s ===" monster.Name
              sprintf "Strength: %d" monster.Strength
              "        * *"
              "       *****"
              "      *******"
              "        * *" ]

let gateContent stage =
    let lOp = describeOperation stage.LeftGate.Operation
    let rOp = describeOperation stage.RightGate.Operation

    [ ("\u256d\u2500\u2500\u2500\u2500 L GATE \u2500\u2500\u2500\u2500\u256e", blueBg, "\u256d\u2500\u2500\u2500\u2500 R GATE \u2500\u2500\u2500\u2500\u256e", blueBg)
      (sprintf "\u2502      %-7s      \u2502" lOp, blueBg,
       sprintf "\u2502      %-7s      \u2502" rOp, blueBg)
      ("\u2502   choose lane    \u2502", blueBg, "\u2502   choose lane    \u2502", blueBg)
      ("\u2570\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u256f", blueBg,
       "\u2570\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u256f", blueBg) ]

let printMap currentStage =
    printf "    Map: "

    for i in 1 .. totalStages do
        let stage = stages.[i - 1]
        let isBoss = match stage.Monster with Some m -> m.IsFinalBoss | None -> false
        let isMon = Option.isSome stage.Monster

        let label =
            if isBoss then "B"
            elif isMon then "!"
            else string i

        if i = currentStage then
            printf "%s" (yellow (sprintf ">%s<" label))
        elif i < currentStage then
            printf "%s" (green (sprintf "[%s]" label))
        elif isMon || isBoss then
            printf "%s" (red (sprintf "[%s]" label))
        else
            printf "%s" (dim (sprintf "[%s]" label))

        if i < totalStages then
            printf "%s" (dim "--")

    printfn ""

let printHUD soldiers stageNum =
    let soldierStyle =
        if soldiers >= 80 then green
        elif soldiers >= 25 then yellow
        else red

    printfn
        "    %s %s    %s %s"
        (cyan "Stage:")
        (yellow (sprintf "%d/%d" stageNum totalStages))
        (cyan "Soldiers:")
        (soldierStyle (sprintf "%d" soldiers))

let printTitleScreen () =
    clearScreen ()
    printfn ""
    printfn ""
    let line = String.replicate 49 "\u2550"
    printfn "    %s" (cyan line)
    printfn ""
    printfn "         %s" (gold "A R I T H M E T I C    A R M Y")
    printfn ""
    printfn "    %s" (cyan line)
    printfn ""
    printfn "    %s" (dim "  Choose your gates wisely. Build your army.")
    printfn "    %s" (dim "  Defeat the Dragon Lord at Stage 15!")
    printfn ""
    printfn "    Soldiers: %s    Stages: %s    Bosses: %s" (green "10") (yellow (string totalStages)) (red "5")
    printfn ""
    printfn "    Controls: %s = Left Lane    %s = Right Lane    %s = Pass Now" (cyan "[A]") (cyan "[D]") (cyan "[SPACE]")
    printfn ""
    printf "    Press ENTER to begin... "
    Console.ReadLine() |> ignore

let printStageView soldiers stage frame gateTop armyLane message =
    clearScreen ()
    let gates = gateContent stage
    let monsterTop = 4 + min 6 (frame / 3)
    let armyTop = 17

    printfn ""
    printfn "    %s" (gold "ARITHMETIC ARMY")
    printHUD soldiers stage.Number
    printMap stage.Number
    printfn ""
    printRoadTop ()
    printLaneLine "\u25c0 LEFT LANE" cyan "RIGHT LANE \u25b6" cyan

    let army = armyRows soldiers frame

    let monster =
        match stage.Monster with
        | Some m -> monsterArt m
        | None -> []

    for row in 1 .. 20 do
        let gateOff = row - gateTop
        let monsterOff = row - monsterTop
        let armyOff = row - armyTop

        if gateOff >= 0 && gateOff < gates.Length then
            let (lt, ls, rt, rs) = gates.[gateOff]
            printLaneLine lt ls rt rs
        elif monsterOff >= 0 && monsterOff < monster.Length then
            printWideLine monster.[monsterOff] red
        elif armyOff >= 0 && armyOff < army.Length then
            match armyLane with
            | LeftLane -> printLaneLine army.[armyOff] green "" laneBg
            | RightLane -> printLaneLine "" laneBg army.[armyOff] green
        elif row = 7 && Option.isNone stage.Monster then
            printWideLine "~ clear road ahead ~" dim
        else
            printBlankLane frame row

    printRoadBottom ()
    printfn ""
    printfn "    %s" (dim "Move army: [A] Left  [D] Right  [SPACE] Pass now")

    match message with
    | Some text -> printfn "    %s" text
    | None -> ()

let laneName lane =
    match lane with
    | LeftLane -> "left"
    | RightLane -> "right"

let choiceName lane =
    match lane with
    | LeftLane -> "L"
    | RightLane -> "R"

let rec readLaneChoice () =
    printf "    Choose lane (A/D): "

    match Console.ReadLine() with
    | null -> None
    | input ->
        match input.Trim().ToUpperInvariant() with
        | "A" | "L" -> Some LeftLane
        | "D" | "R" -> Some RightLane
        | _ ->
            printfn "    Invalid input. Please enter A or D."
            readLaneChoice ()

let runGateApproach soldiers stage =
    let gateBottomTop = 16
    let frameDelay = max 45 (150 - (stage.Number * 10))

    let gateAdvanceEvery =
        if stage.Number < 5 then 2 else 1

    if Console.IsInputRedirected || Console.IsOutputRedirected then
        printStageView soldiers stage 0 gateBottomTop LeftLane None
        readLaneChoice ()
    else
        let mutable frame = 0
        let mutable gateTop = 1
        let mutable lane = LeftLane
        let mutable invalidFrames = 0
        let mutable passedGate = false

        while not passedGate do
            let msg =
                if invalidFrames > 0 then
                    Some(orange "Invalid key! Use A, D, or Space.")
                else
                    Some(sprintf "Army lane: %s" (cyan (laneName lane)))

            printStageView soldiers stage frame gateTop lane msg

            if invalidFrames > 0 then
                invalidFrames <- invalidFrames - 1

            if gateTop >= gateBottomTop then
                Thread.Sleep 250
                passedGate <- true
            else
                let frameEnd = DateTime.UtcNow.AddMilliseconds(float frameDelay)

                while not passedGate && DateTime.UtcNow < frameEnd do
                    if Console.KeyAvailable then
                        let key = Console.ReadKey(true)

                        match Char.ToUpperInvariant(key.KeyChar) with
                        | 'A' -> lane <- LeftLane
                        | 'D' -> lane <- RightLane
                        | ' ' -> passedGate <- true
                        | '\r' | '\n' -> ()
                        | _ -> invalidFrames <- 5
                    else
                        Thread.Sleep 15

                frame <- frame + 1

                if frame % gateAdvanceEvery = 0 then
                    gateTop <- gateTop + 1

        Some lane

let strengthBar maxVal value =
    let barW = 20

    let filled =
        if maxVal <= 0 then 0
        else min barW (max 1 (value * barW / maxVal))

    let empty = barW - filled
    sprintf "[%s%s]" (String.replicate filled "#") (String.replicate empty ".")

let printBattleScene soldiers monster =
    clearScreen ()
    printfn ""
    let line = String.replicate 42 "\u2550"
    printfn "    %s" (red line)
    printfn ""
    printfn "              %s" (red "! ! !  BATTLE  ! ! !")
    printfn ""
    printfn "    %s" (red line)
    printfn ""

    for artLine in monsterArt monster do
        printfn "    %s" (red (sprintf "        %s" artLine))

    printfn ""
    printfn "    %s %s" (red "    Monster:") (red (strengthBar 200 monster.Strength))
    printfn ""
    printfn "    %s" (orange "            vvv ATTACK vvv")
    printfn ""

    let army = armyRows soldiers 0

    for artLine in army do
        printfn "    %s" (green (sprintf "        %s" artLine))

    printfn ""
    printfn "    %s %s %s" (green "    Army:   ") (green (strengthBar 200 soldiers)) (green (sprintf "%d soldiers" soldiers))
    printfn ""
    printfn "    %s" (red line)

let resolveMonster stageNumber soldiers monster =
    printBattleScene soldiers monster
    Thread.Sleep 1500

    if soldiers > monster.Strength then
        let remaining = soldiers - monster.Strength
        let log = sprintf "Stage %d: defeated %s. Remaining soldiers: %d." stageNumber monster.Name remaining

        printfn ""
        printfn "    %s" (green (sprintf "Victory! Defeated %s!" monster.Name))
        Thread.Sleep 1500

        if monster.IsFinalBoss then
            Finished(true, remaining, [ log ])
        else
            Continue(remaining, false, [ log ])
    else
        let log = sprintf "Stage %d: lost to %s." stageNumber monster.Name

        printfn ""
        printfn "    %s" (red (sprintf "Defeated by %s!" monster.Name))
        Thread.Sleep 1500

        Finished(false, soldiers, [ log ])

let playStage soldiers pendingPenalty stage =
    let leftResult = applyOperation soldiers stage.LeftGate.Operation
    let rightResult = applyOperation soldiers stage.RightGate.Operation

    match runGateApproach soldiers stage with
    | None ->
        Finished(false, soldiers, [ sprintf "Stage %d: input ended." stage.Number ])
    | Some lane ->
        let gate =
            if lane = LeftLane then stage.LeftGate else stage.RightGate

        let updated = applyOperation soldiers gate.Operation
        let bestResult = max leftResult rightResult
        let choseBest = updated = bestResult

        let formatGateOption optionLane label operation =
            let text = sprintf "%s %s" label (describeOperation operation)

            if lane = optionLane then
                green text
            else
                dim text

        let leftGateOption = formatGateOption LeftLane "L" stage.LeftGate.Operation
        let rightGateOption = formatGateOption RightLane "R" stage.RightGate.Operation

        let gateLog =
            sprintf
                "Stage %d: Gate options: %s / %s. Soldiers became %d, optimal: %b."
                stage.Number
                leftGateOption
                rightGateOption
                updated
                choseBest

        let choiceLogs = [ gateLog ]

        if updated <= 0 then
            Finished(
                false,
                0,
                [ yield! choiceLogs
                  sprintf "Stage %d: soldiers reached zero. Game over." stage.Number ]
            )
        else
            match stage.Monster with
            | Some monster ->
                let checkpointPenalty = pendingPenalty || not choseBest

                let adjustedMonster =
                    if checkpointPenalty then
                        { monster with Strength = updated + 1 }
                    else
                        monster

                let checkpointLogs =
                    if checkpointPenalty then
                        [ sprintf "Stage %d: non-optimal route empowered %s at the checkpoint." stage.Number monster.Name ]
                    else
                        []

                match resolveMonster stage.Number updated adjustedMonster with
                | Continue(r, _, logs) ->
                    Continue(r, false, choiceLogs @ checkpointLogs @ logs)
                | Finished(w, fs, logs) ->
                    Finished(w, fs, choiceLogs @ checkpointLogs @ logs)

            | None ->
                Continue(updated, pendingPenalty || not choseBest, choiceLogs)

let rec play soldiers remainingStages pendingPenalty logs =
    match remainingStages with
    | [] -> false, 0, logs @ [ "No final boss found. Game ended." ]
    | stage :: rest ->
        match playStage soldiers pendingPenalty stage with
        | Continue(s, nextPenalty, sl) -> play s rest nextPenalty (logs @ sl)
        | Finished(w, fs, sl) -> w, fs, logs @ sl

let printVictory finalSoldiers =
    clearScreen ()
    printfn ""
    printfn ""
    let line = String.replicate 49 "\u2550"
    printfn "    %s" (gold line)
    printfn ""
    printfn "        %s" (gold "  *  *  *  V I C T O R Y  *  *  *")
    printfn ""
    printfn "        %s" (green "  The Dragon Lord has been slain!")
    printfn "        %s" (cyan (sprintf "  %d soldiers survived." finalSoldiers))
    printfn ""
    printfn "    %s" (gold line)
    printfn ""

let printDefeat () =
    clearScreen ()
    printfn ""
    printfn ""
    let line = String.replicate 49 "\u2550"
    printfn "    %s" (red line)
    printfn ""
    printfn "           %s" (red "G A M E    O V E R")
    printfn ""
    printfn "           %s" (dim "Your army has fallen...")
    printfn ""
    printfn "    %s" (red line)
    printfn ""

type LeaderboardEntry =
    { Elapsed: TimeSpan
      CompletedAt: DateTime
      Soldiers: int }

let leaderboardPath = Path.Combine(Environment.CurrentDirectory, "leaderboard.txt")

let formatElapsed (elapsed: TimeSpan) =
    sprintf "%02d:%02d.%03d" (int elapsed.TotalMinutes) elapsed.Seconds elapsed.Milliseconds

let tryParseLeaderboardEntry (line: string) =
    let parts = line.Split('|')

    if parts.Length <> 3 then
        None
    else
        let mutable ticks = 0L
        let mutable completedAt = DateTime.MinValue
        let mutable soldiers = 0

        if
            Int64.TryParse(parts.[0], &ticks)
            && DateTime.TryParse(parts.[1], &completedAt)
            && Int32.TryParse(parts.[2], &soldiers)
        then
            Some
                { Elapsed = TimeSpan ticks
                  CompletedAt = completedAt
                  Soldiers = soldiers }
        else
            None

let loadLeaderboard () =
    try
        if File.Exists leaderboardPath then
            File.ReadAllLines leaderboardPath
            |> Array.choose tryParseLeaderboardEntry
            |> Array.toList
        else
            []
    with _ ->
        []

let topLeaderboard entries =
    entries
    |> List.sortBy (fun entry -> entry.Elapsed)
    |> List.truncate 5

let saveLeaderboard entries =
    try
        let lines =
            entries
            |> topLeaderboard
            |> List.map (fun entry ->
                sprintf "%d|%s|%d" entry.Elapsed.Ticks (entry.CompletedAt.ToString("O")) entry.Soldiers)

        File.WriteAllLines(leaderboardPath, lines)
    with _ ->
        ()

let updateLeaderboard won elapsed finalSoldiers =
    let entries = loadLeaderboard ()

    if won then
        let updated =
            { Elapsed = elapsed
              CompletedAt = DateTime.Now
              Soldiers = finalSoldiers }
            :: entries
            |> topLeaderboard

        saveLeaderboard updated
        updated
    else
        topLeaderboard entries

let printLeaderboard entries =
    printfn ""
    printfn "    %s" (cyan "Time Attack Leaderboard")
    printfn "    %s" (dim (String.replicate 45 "\u2500"))

    if List.isEmpty entries then
        printfn "    %s" (dim "No victory records yet.")
    else
        entries
        |> List.iteri (fun index entry ->
            printfn
                "    %d. %s   %d soldiers   %s"
                (index + 1)
                (gold (formatElapsed entry.Elapsed))
                entry.Soldiers
                (dim (entry.CompletedAt.ToString("yyyy-MM-dd HH:mm"))))

let printGameSummary won finalSoldiers elapsed gameLogs =
    if won then printVictory finalSoldiers else printDefeat ()

    printfn ""
    printfn "    %s %s" (cyan "Run Time:") (gold (formatElapsed elapsed))

    let leaderboard = updateLeaderboard won elapsed finalSoldiers
    printLeaderboard leaderboard

    printfn ""
    printfn "    %s" (cyan "Game Log:")
    printfn "    %s" (dim (String.replicate 45 "\u2500"))

    gameLogs
    |> List.iter (fun s -> printfn "    %s %s" (dim "-") s)

    printfn ""

let rec readReplayChoice () =
    printf "    Play again? %s/%s: " (cyan "R") (cyan "Q")

    match Console.ReadLine() with
    | null -> false
    | input ->
        match input.Trim().ToUpperInvariant() with
        | "R"
        | "Y"
        | "YES" -> true
        | "Q"
        | "N"
        | "NO"
        | "" -> false
        | _ ->
            printfn "    Please enter R to replay or Q to quit."
            readReplayChoice ()

let runSingleGame () =
    stages <- newGameStages ()
    printTitleScreen ()

    let timer = Stopwatch.StartNew()
    let won, finalSoldiers, gameLogs = play 10 stages false []
    timer.Stop()

    printGameSummary won finalSoldiers timer.Elapsed gameLogs

let rec gameLoop () =
    runSingleGame ()

    if readReplayChoice () then
        gameLoop ()
    else
        printfn ""
        printfn "    %s" (dim "Thanks for playing Arithmetic Army.")

gameLoop ()
