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
let dim t = paint "90" t
let darkDim t = paint "90" t
let gold t = paint "33;1" t
let orange t = paint "33" t
let purple t = paint "35;1" t
let laneBg = dim
let gateGood t = paint "32;1" t
let gateBad t = paint "31;1" t
let titleRed t = paint "31;1" t
let titleOrange t = paint "33;1" t

let fullClearScreen () =
    if not Console.IsOutputRedirected then
        try Console.Clear() with _ -> ()

let resetCursor () =
    if not Console.IsOutputRedirected then
        try Console.SetCursorPosition(0, 0) with _ -> ()

let hideCursor () =
    if not Console.IsOutputRedirected then
        try Console.CursorVisible <- false with _ -> ()

let showCursor () =
    if not Console.IsOutputRedirected then
        try Console.CursorVisible <- true with _ -> ()

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

let armyRows soldiers frame =
    let units = max 1 (min 15 ((soldiers + 4) / 5))
    let s = if frame % 2 = 0 then " " else ""
    let s2 = if frame % 2 = 0 then "" else " "
    
    let r1 = units / 3
    let r2 = units / 2
    let r3 = units - r1 - r2
    
    [ sprintf "   [ ARMY : %-4d ]" soldiers
      s + "   " + String.replicate (max 1 r1) " \u25b2 "
      s2 + "  " + String.replicate (max 1 r2) " \u25b2 "
      s + " " + String.replicate (max 1 r3) " \u25b2 " ]

let monsterArt monster =
    if monster.IsFinalBoss then
        [ sprintf "=== %s ===" (purple monster.Name)
          sprintf "Strength: %d" monster.Strength
          "             \\|/          "
          "            .-*-._        "
          "      /\\   /  _   \\   /\\  "
          "     /  \\_(  (0)   )_/  \\ "
          "    /      \\      /      \\"
          "   /        \\____/        \\"
          "  /  /\\  \\        /  /\\  \\ "
          " /  /  \\  \\======/  /  \\  \\"
          "   /    \\   ||||   /    \\  "
          "            ||||          " ]
    else
        match monster.Name with
        | "Goblin Horde" ->
            [ sprintf "=== %s ===" (green monster.Name)
              sprintf "Strength: %d" monster.Strength
              "    /\\   /\\   /\\   /\\   /\\  "
              "   (oo) (oo) (oo) (oo) (oo) "
              "   /||\\ /||\\ /||\\ /||\\ /||\\ "
              "    /\\   /\\   /\\   /\\   /\\  " ]
        | "Stone Golem" ->
            [ sprintf "=== %s ===" (dim monster.Name)
              sprintf "Strength: %d" monster.Strength
              "         _______         "
              "        |       |        "
              "      __| \u2588   \u2588 |__      "
              "     |  |_______|  |     "
              "     |__|       |__|     "
              "        |_______|        "
              "        | |   | |        "
              "        |_|   |_|        " ]
        | "Arcane Swarm" ->
            [ sprintf "=== %s ===" (cyan monster.Name)
              sprintf "Strength: %d" monster.Strength
              "        .*.   .*.        "
              "     .*. * *.* * .*.     "
              "      *   *   *   *      "
              "     .*. * *.* * .*.     "
              "        .*.   .*.        " ]
        | "Iron Colossus" ->
            [ sprintf "=== %s ===" (orange monster.Name)
              sprintf "Strength: %d" monster.Strength
              "         [+|-|+]         "
              "        /| [O] |\\        "
              "       | |_____| |       "
              "       |=|  |  |=|       "
              "       |/ \\_|_/ \\|       "
              "          [| |]          "
              "          [| |]          "
              "          /| |\\          " ]
        | _ ->
            [ sprintf "=== %s ===" monster.Name
              sprintf "Strength: %d" monster.Strength
              "        * *"
              "       *****"
              "      *******"
              "        * *" ]

let gateStyle op =
    match op with
    | Add _ | Multiply _ -> gateGood
    | Subtract _ | Divide _ -> gateBad

let gateContent stage =
    let lOp = describeOperation stage.LeftGate.Operation
    let rOp = describeOperation stage.RightGate.Operation
    let lBg = gateStyle stage.LeftGate.Operation
    let rBg = gateStyle stage.RightGate.Operation

    [ ("\u250c\u2500\u2500[ LEFT  GATE ]\u2500\u2500\u2500\u2510", lBg, "\u250c\u2500\u2500[ RIGHT GATE ]\u2500\u2500\u2500\u2510", rBg)
      ("\u2502                     \u2502", lBg, "\u2502                     \u2502", rBg)
      (sprintf "\u2502       %-7s       \u2502" lOp, lBg,
       sprintf "\u2502       %-7s       \u2502" rOp, rBg)
      ("\u2502                     \u2502", lBg, "\u2502                     \u2502", rBg)
      ("\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518", lBg,
       "\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518", rBg) ]

let printMap currentStage =
    printf "    %s " (cyan "Map:")

    for i in 1 .. totalStages do
        let stage = stages.[i - 1]
        let isBoss = match stage.Monster with Some m -> m.IsFinalBoss | None -> false
        let isMon = Option.isSome stage.Monster

        if i = currentStage then
            printf "%s" (yellow "\u25cf")
        elif i < currentStage then
            printf "%s" (green "\u25cf")
        elif isBoss then
            printf "%s" (purple "\u25cf")
        elif isMon then
            printf "%s" (orange "\u25cf")
        else
            printf "%s" (dim "\u25cb")

        if i < totalStages then
            if i < currentStage then
                printf "%s" (green "\u2500")
            else
                printf "%s" (dim "\u2500")

    printfn ""

let printHUD soldiers stageNum =
    let soldierStyle =
        if soldiers >= 80 then green
        elif soldiers >= 25 then yellow
        else red

    let sBox = sprintf " %s %s " (cyan "Soldiers:") (soldierStyle (sprintf "%-4d" soldiers))
    let stBox = sprintf " %s %s " (cyan "Stage:") (yellow (sprintf "%-5s" (sprintf "%d/%d" stageNum totalStages)))
    
    printfn "    %s" (dim "\u250c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u252c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2510")
    printfn "    %s%s%s%s%s" (dim "\u2502") stBox (dim "\u2502") sBox (dim "\u2502")
    printfn "    %s" (dim "\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2534\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518")

let printTitleScreen () =
    fullClearScreen ()
    hideCursor ()
    printfn ""
    printfn "    %s" (titleRed "   __   ___  _ _____ _   _ __  __ ___ _____ ___ ___ ")
    printfn "    %s" (titleRed "  / /\\ | _ \\| |_   _| |_| |  \\/  | __|_   _|_ _/ __|")
    printfn "    %s" (titleOrange " / /__\\|   /| | | | |  _  | |\\/| | _|  | |  | | (__ ")
    printfn "    %s" (titleOrange "/_/    \\_|_\\|_| |_| |_| |_|_|  |_|___| |_| |___\\___|")
    printfn "    %s" (gold "       _   ___ __  __ __   __                       ")
    printfn "    %s" (gold "      / \\ | _ \\  \\/  |\\ \\ / /                       ")
    printfn "    %s" (yellow "     / _ \\|   / |\\/| | \\ V /                        ")
    printfn "    %s" (yellow "    /_/ \\_\\_|_\\_|  |_|  |_|                         ")
    printfn ""
    let line = String.replicate 56 "\u2550"
    printfn "    %s" (dim line)
    printfn "    %s" (dim "      Choose your gates wisely. Build your army.")
    printfn "    %s" (dim "      Defeat the Dragon Lord at Stage 15!")
    printfn "    %s" (dim line)
    printfn ""
    printfn "    Soldiers: %s    Stages: %s    Bosses: %s" (green "10") (yellow (string totalStages)) (red "5")
    printfn ""
    printfn "    Controls: %s = Left Lane    %s = Right Lane    %s = Pass Now" (cyan "[A]") (cyan "[D]") (cyan "[SPACE]")
    printfn ""
    printf "    Press ENTER to begin... "
    showCursor ()
    Console.ReadLine() |> ignore
    hideCursor ()
    fullClearScreen ()

let printStageView soldiers stage frame gateTop armyLane message =
    resetCursor ()
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
    | Some text -> printfn "    %-60s" text
    | None -> printfn "                                                                "

let laneName lane =
    match lane with
    | LeftLane -> "left"
    | RightLane -> "right"

let choiceName lane =
    match lane with
    | LeftLane -> "L"
    | RightLane -> "R"

let rec readLaneChoice () =
    showCursor ()
    printf "    Choose lane (A/D): "

    match Console.ReadLine() with
    | null -> 
        hideCursor ()
        None
    | input ->
        match input.Trim().ToUpperInvariant() with
        | "A" | "L" -> 
            hideCursor ()
            Some LeftLane
        | "D" | "R" -> 
            hideCursor ()
            Some RightLane
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

        fullClearScreen ()

        let printCurrentView () =
            let msg =
                if invalidFrames > 0 then
                    Some(orange "Invalid key! Use A, D, or Space.")
                else
                    Some(sprintf "Army lane: %s" (cyan (laneName lane)))
            printStageView soldiers stage frame gateTop lane msg
            if invalidFrames > 0 then invalidFrames <- invalidFrames - 1

        printCurrentView ()
        let mutable nextFrameTime = DateTime.UtcNow.AddMilliseconds(float frameDelay)

        while not passedGate do
            let now = DateTime.UtcNow
            let mutable dirty = false

            if now >= nextFrameTime then
                if gateTop >= gateBottomTop then
                    Thread.Sleep 250
                    passedGate <- true
                else
                    frame <- frame + 1
                    if frame % gateAdvanceEvery = 0 then
                        gateTop <- gateTop + 1
                    nextFrameTime <- now.AddMilliseconds(float frameDelay)
                    dirty <- true

            while Console.KeyAvailable do
                let key = Console.ReadKey(true)
                let prevLane = lane
                match Char.ToUpperInvariant(key.KeyChar) with
                | 'A' -> lane <- LeftLane
                | 'D' -> lane <- RightLane
                | ' ' -> passedGate <- true
                | '\r' | '\n' -> ()
                | _ -> invalidFrames <- 5

                if prevLane <> lane || invalidFrames > 0 then
                    dirty <- true

            if dirty && not passedGate then
                printCurrentView ()

            if not passedGate then
                Thread.Sleep 2

        Some lane

let strengthBar maxVal value =
    let barW = 20

    let filled =
        if maxVal <= 0 then 0
        else min barW (max 1 (value * barW / maxVal))

    let empty = barW - filled
    sprintf "[%s%s]" (String.replicate filled "#") (String.replicate empty ".")

let printBattleScene soldiers monster showFlash =
    resetCursor ()
    printfn ""
    let line = String.replicate 42 "\u2550"
    let titleColor = if showFlash then yellow else red
    let armyColor = if showFlash then cyan else green
    printfn "    %s" (titleColor line)
    printfn ""
    printfn "              %s" (titleColor "! ! !  BATTLE  ! ! !")
    printfn ""
    printfn "    %s" (titleColor line)
    printfn ""

    for artLine in monsterArt monster do
        let c = if showFlash then titleColor else red
        printfn "    %s" (c (sprintf "        %s" artLine))

    printfn ""
    printfn "    %s %s" (red "    Monster:") (red (strengthBar 200 monster.Strength))
    printfn ""
    printfn "    %s" (orange "            vvv ATTACK vvv")
    printfn ""

    let army = armyRows soldiers 0

    for artLine in army do
        printfn "    %s" (armyColor (sprintf "        %s" artLine))

    printfn ""
    printfn "    %s %s %s" (green "    Army:   ") (green (strengthBar 200 soldiers)) (green (sprintf "%d soldiers        " soldiers))
    printfn ""
    printfn "    %s" (titleColor line)
    printfn "                                                                "
    printfn "                                                                "

let resolveMonster stageNumber soldiers monster =
    fullClearScreen ()
    for i in 1 .. 5 do
        printBattleScene soldiers monster (i % 2 = 0)
        Thread.Sleep 150
    Thread.Sleep 500

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

        let uncoloredLeft = sprintf "L %s" (describeOperation stage.LeftGate.Operation)
        let uncoloredRight = sprintf "R %s" (describeOperation stage.RightGate.Operation)
        let leftOptionColored = if lane = LeftLane then green uncoloredLeft else dim uncoloredLeft
        let rightOptionColored = if lane = RightLane then green uncoloredRight else dim uncoloredRight
        let selectedGateName = if lane = LeftLane then "L" else "R"

        let gateLog =
            sprintf
                "Stage %d: Gate options: %s / %s. Selected gate: %s. Resulting soldier count: %d, optimal: %b."
                stage.Number
                leftOptionColored
                rightOptionColored
                selectedGateName
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
    fullClearScreen ()
    printfn ""
    printfn "    %s" (gold " __   __ ___  ___  _____  ___   ___  __   __ ")
    printfn "    %s" (gold " \\ \\ / /|_ _|/ __||_   _|/ _ \\ | _ \\ \\ \\ / / ")
    printfn "    %s" (yellow "  \\ V /  | || (__   | | | (_) ||   /  \\ V /  ")
    printfn "    %s" (yellow "   \\_/  |___|\\___|  |_|  \\___/ |_|_\\   |_|   ")
    printfn ""
    let line = String.replicate 49 "\u2550"
    printfn "    %s" (gold line)
    printfn "        %s" (green "  The Dragon Lord has been slain!")
    printfn "        %s" (cyan (sprintf "  %d soldiers survived." finalSoldiers))
    printfn "    %s" (gold line)
    printfn ""

let printDefeat () =
    fullClearScreen ()
    printfn ""
    printfn "    %s" (red "   ___   _   __  __ ___    _____   _____ ___  ")
    printfn "    %s" (red "  / __| /_\\ |  \\/  | __|  / _ \\ \\ / / __| _ \\ ")
    printfn "    %s" (red " | (_ |/ _ \\| |\\/| | _|  | (_) \\ V /| _||   / ")
    printfn "    %s" (red "  \\___/_/ \\_\\_|  |_|___|  \\___/ \\_/ |___|_|_\\ ")
    printfn ""
    let line = String.replicate 49 "\u2550"
    printfn "    %s" (red line)
    printfn "           %s" (dim "Your army has fallen...")
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
        let newEntry = 
            { Elapsed = elapsed
              CompletedAt = DateTime.Now
              Soldiers = finalSoldiers }
              
        let updated =
            newEntry :: entries
            |> topLeaderboard

        saveLeaderboard updated
        updated, Some newEntry
    else
        topLeaderboard entries, None

let blink t = paint "5;32;1" t

let printLeaderboard entries newEntryOpt =
    printfn ""
    printfn "    %s" (cyan "Time Attack Leaderboard")
    printfn "    %s" (dim (String.replicate 45 "\u2500"))

    if List.isEmpty entries then
        printfn "    %s" (dim "No victory records yet.")
    else
        entries
        |> List.iteri (fun index entry ->
            let isNew = match newEntryOpt with Some n -> n = entry | None -> false
            
            let timeStr = formatElapsed entry.Elapsed
            let formattedTime = if isNew then blink timeStr else gold timeStr
            let tag = if isNew then blink "  <<< NEW RECORD!" else ""
            
            printfn
                "    %d. %s   %d soldiers   %s%s"
                (index + 1)
                formattedTime
                entry.Soldiers
                (dim (entry.CompletedAt.ToString("yyyy-MM-dd HH:mm")))
                tag)

let printGameSummary won finalSoldiers elapsed gameLogs =
    if won then printVictory finalSoldiers else printDefeat ()

    printfn ""
    printfn "    %s %s" (cyan "Play Time:") (gold (formatElapsed elapsed))

    let leaderboard, newEntryOpt = updateLeaderboard won elapsed finalSoldiers
    printLeaderboard leaderboard newEntryOpt

    printfn ""
    printfn "    %s" (cyan "Game Log:")
    printfn "    %s" (dim (String.replicate 45 "\u2500"))

    gameLogs
    |> List.iter (fun s -> printfn "    %s %s" (dim "-") s)

    printfn ""

let rec readReplayChoice () =
    showCursor ()
    printf "    Play again? %s/%s: " (cyan "R") (cyan "Q")

    match Console.ReadLine() with
    | null -> 
        hideCursor ()
        false
    | input ->
        match input.Trim().ToUpperInvariant() with
        | "R"
        | "Y"
        | "YES" -> 
            hideCursor ()
            true
        | "Q"
        | "N"
        | "NO" -> 
            hideCursor ()
            false
        | ""
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
