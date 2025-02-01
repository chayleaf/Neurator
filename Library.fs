namespace Neurator

open System
open System.Reflection
open HarmonyLib
open NeuroFSharp
open RimWorld

type EventCtx =
    { categoryName: string
      eventName: string
      description: string
      pointsCost: int
      enoughPoints: bool }

type EventsCtx =
    { yourPoints: int
      events: EventCtx list }

type Actions =
    | [<Action("spawn_event", "Spawn an event. You must have enough points to spawn the event.")>] SpawnEvent of
        eventName: string
    | [<Action("list_events", "List events and their costs in points. You must select the event categories.")>] ListEvents of
        categoryNames: string list

type StorytellerComp_NeuroSama() =
    inherit StorytellerComp()
    let mutable neuroPoints = 0f
    let mutable lastTarget = null
    let mutable eventsUntriggered = 0

    let randy () =
        Verse.Find.Storyteller.storytellerComps
        |> Seq.pick (function
            | :? StorytellerComp_RandomMain as x -> Some x
            | _ -> None)

    let randyProps () =
        (randy ()).props :?> StorytellerCompProperties_RandomMain

    member this.Props = this.props :?> StorytellerCompProperties_NeuroSama

    member this.UsableIncidentsInCategory'(a, b: IncidentParms) = this.UsableIncidentsInCategory(a, b)
    member this.IncidentChanceFinal'(def, target) = this.IncidentChanceFinal(def, target)

    member _.Points
        with get () = neuroPoints
        and set value = neuroPoints <- value

    member this.IncidentPoints(target: IIncidentTarget) =
        let randyProps = randyProps ()

        let cats' =
            randyProps.categoryWeights
            |> Seq.map (fun cat ->
                let parms = StorytellerUtility.DefaultParmsNow(cat.category, target)

                let incs =
                    this.UsableIncidentsInCategory'(cat.category, parms)
                    |> Seq.map (fun def -> def, this.IncidentChanceFinal'(def, target))
                    |> List.ofSeq

                cat, incs)
            |> Seq.filter (snd >> List.isEmpty >> not)
            |> List.ofSeq

        let cats =
            cats' |> Seq.map (fun (cat, _) -> cat.category.defName, cat.weight) |> Map.ofSeq

        let ws =
            cats'
            |> List.collect (fun (cat, incs) ->
                let w = Map.tryFind cat.category.defName cats |> Option.defaultValue 0f
                incs |> List.map (fun (d, c) -> d, c * w))

        let wTot = ws |> Seq.map snd |> Seq.fold (+) 0f
        ws |> List.filter (snd >> (<>) 0f) |> List.map (fun (inc, w) -> inc, wTot / w)

    member this.EatRandyIncidents (incidents: FiringIncident seq) (target: IIncidentTarget) =
        let pm =
            this.IncidentPoints target
            |> Seq.map (fun (inc, w) -> inc.defName, w)
            |> Map.ofSeq

        incidents
        |> Seq.iter (fun inc ->
            let points = Map.tryFind inc.def.defName pm |> Option.defaultValue 0f

            let points =
                points
                * (match eventsUntriggered with
                   | 0 -> 1.5f
                   | 1 -> 1.0f
                   | 2 -> 0.5f
                   | _ -> 0.0f)

            Game.Instance.ReportPointsGain
                (int (Math.Ceiling(float neuroPoints + float points))
                 - int (Math.Ceiling(float neuroPoints)))
                eventsUntriggered

            eventsUntriggered <- eventsUntriggered + 1
            neuroPoints <- neuroPoints + points)

    member this.RefreshAvailableIncidents() =
        try
            if lastTarget <> null then
                Game.Instance.SetAvailableIncidents(this.IncidentPoints lastTarget)
        with exc ->
            Game.Instance.LogError(exc.ToString())

    override this.MakeIntervalIncidents(target) =
        match target with
        | :? Verse.Map as map when Seq.contains IncidentTargetTagDefOf.Map_PlayerHome (map.IncidentTargetTags()) ->
            lastTarget <- target
            this.RefreshAvailableIncidents()

            let queue = Game.Instance.FlushQueue()

            if not (List.isEmpty queue) then
                eventsUntriggered <- 0

            queue
            |> List.map (fun def ->
                Game.Instance.ReportEventTriggered def
                FiringIncident(def, this, this.GenerateParms(def.category, target)))
            |> Seq.ofList
        | _ -> Seq.empty

    override _.GenerateParms(incCat, target) =
        let ret = StorytellerUtility.DefaultParmsNow(incCat, target)

        if ret.points >= 0f then
            ret.points <- ret.points * (randyProps ()).randomPointsFactorRange.RandomInRange

        ret

and StorytellerCompProperties_NeuroSama() as this =
    inherit StorytellerCompProperties()
    do this.compClass <- typeof<StorytellerComp_NeuroSama>

and [<Verse.StaticConstructorOnStartup>] Game() as this =
    inherit Game<Actions>()

    let cts = new Threading.CancellationTokenSource()
    let mutable harmony = Harmony("org.pavluk.neurator")
    let mutable queuedIncidents: IncidentDef list = []
    let mutable availableIncidents = []
    static let instance: Game = Game()

    let neuro () =
        if Verse.Find.Storyteller = null || Verse.Find.Storyteller.storytellerComps = null then
            None
        else
            Verse.Find.Storyteller.storytellerComps
            |> Seq.tryPick (function
                | :? StorytellerComp_NeuroSama as x -> Some x
                | _ -> None)

    do
        try
            harmony.PatchAll(Assembly.GetExecutingAssembly())
            let cnt = Seq.fold (fun x _ -> x + 1) 0 (harmony.GetPatchedMethods())
            this.Start(None, cts.Token) |> ignore
            this.LogDebug(sprintf "Plugin Neurator is loaded with %d patches!" cnt)
        with exc ->
            this.LogError(sprintf "ERROR %A" exc)

    static member Instance: Game = instance

    member _.FlushQueue() =
        let ret = queuedIncidents |> List.rev
        queuedIncidents <- []
        ret

    member _.SetAvailableIncidents(incidents: (IncidentDef * float32) seq) =
        availableIncidents <- incidents |> Seq.toList

    override this.ReregisterActions() =
        let actions =
            match neuro () with
            | Some neuro ->
                let spawnAct = this.Action SpawnEvent

                spawnAct.MutateProp "eventName" (fun x ->
                    (x :?> StringSchema)
                        .SetEnum(
                            availableIncidents
                            |> List.filter (snd >> (>=) neuro.Points)
                            |> List.map (fst >> _.label)
                            |> Array.ofList
                        ))

                let allCats =
                    availableIncidents
                    |> Seq.map (fst >> _.category.defName)
                    |> Set.ofSeq
                    |> Array.ofSeq
                    |> Array.sort

                let listAct = this.Action ListEvents

                listAct.MutateProp "categoryNames" (fun x ->
                    ((x :?> ArraySchema).Items :?> StringSchema).SetEnum(allCats))

                [ spawnAct; listAct ]
            | None -> []

        this.RetainActions(actions |> List.map (fun x -> x))

    override _.Name = "RimWorld"

    member _.ReportEventTriggered(def: IncidentDef) =
        Printf.ksprintf (this.Context false) "The event \"%s\ has been successfully triggered!" def.label

    member _.ReportPointsGain (points: int) (debuff: int) =
        Printf.ksprintf
            (this.Context true)
            (match debuff with
             | 0 -> "You got %d points"
             | 1 -> "You got %d points. The yield was reduced because you haven't triggered any events in a while."
             | 2 ->
                 "You got %d points. The yield was significantly reduced because you haven't triggered any events in a while."
             | _ -> "You got %d points. You won't get any more points until you start an event.")
            points

    override _.HandleAction(action: Actions) =
        let neuro = neuro ()

        match action with
        | ListEvents categoryNames ->
            match neuro with
            | Some neuro ->
                neuro.RefreshAvailableIncidents()

                let points = neuro.Points

                let incs: EventCtx list =
                    availableIncidents
                    |> List.map (fun (inc, w) ->
                        { categoryName = inc.category.defName
                          eventName = inc.label
                          description = ""
                          enoughPoints = points >= w
                          pointsCost = int (Math.Ceiling(float w)) })
                    |> List.filter (fun ev -> List.contains ev.categoryName categoryNames)

                let ctx =
                    { yourPoints = int (Math.Floor(float points))
                      events = incs }

                Ok(Some(this.Serialize ctx))
            | None -> Error(Some "You can't spawn any events at the moment!")
        | SpawnEvent eventName ->
            match neuro with
            | Some neuro ->
                neuro.RefreshAvailableIncidents()

                let inc = availableIncidents |> List.tryFind (fun (inc, _) -> inc.label = eventName)

                match inc with
                | Some(inc, w) when w <= neuro.Points ->
                    neuro.Points <- neuro.Points - w
                    queuedIncidents <- inc :: queuedIncidents

                    Ok(
                        Some(
                            sprintf
                                "You have spent %d points and have %d points left. This event will be triggered at the next opportunity, you will be notified of it. You can try to keep it a surprise for now! Tip: balance positive and negative events to keep the player on the edge - and don't be afraid to experiment and try new things, the points were balanced so you could trigger much more events than would normally happen in the game!"
                                (int (Math.Ceiling(float w)))
                                (int (Math.Floor(float neuro.Points)))
                        )
                    )
                | Some(_, w) ->
                    Error(
                        Some(
                            sprintf
                                "This event requires %d points to trigger, while you only have %d!"
                                (int (Math.Ceiling(float w)))
                                (int (Math.Floor(float neuro.Points)))
                        )
                    )
                | None ->
                    Error(
                        Some(
                            sprintf
                                "This event is not available. Available events: %s"
                                (this.Serialize(availableIncidents |> List.map (fst >> _.label)))
                        )
                    )
            | None -> Error(Some "You can't spawn any events at the moment!")
    // | Test _countryName -> Error None

    override _.LogError error =
        Printf.ksprintf Verse.Log.Error "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error

    override _.LogDebug error =
        Printf.ksprintf Verse.Log.Warning "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error

    member _.Update() = this.ReregisterActions()
