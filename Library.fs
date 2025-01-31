namespace Neurator

open System
open System.Reflection
open HarmonyLib
open NeuroFSharp
open RimWorld

type Actions = | [<Action("test", "Test.")>] Test of countryName: string

type StorytellerComp_NeuroSama() =
    inherit StorytellerComp()
    let game = Game.Instance

    let randy () =
        Verse.Find.Storyteller.storytellerComps
        |> Seq.pick (function
            | :? StorytellerComp_RandomMain as x -> Some x
            | _ -> None)

    let randyProps () =
        (randy ()).props :?> StorytellerCompProperties_RandomMain

    member this.Props = this.props :?> StorytellerCompProperties_NeuroSama

    override _.MakeIntervalIncidents(target) =
        match target with
        | :? Verse.Map as map when Seq.contains IncidentTargetTagDefOf.Map_PlayerHome (map.IncidentTargetTags()) ->
            let randyProps = randyProps ()
            // let cats = randyProps.categoryWeights |>

            ()
            Seq.empty
        | _ -> Seq.empty

    override _.GenerateParms(incCat, target) =
        let ret = StorytellerUtility.DefaultParmsNow(incCat, target)

        if (ret.points >= 0f) then
            ret.points <- ret.points * (randyProps ()).randomPointsFactorRange.RandomInRange

        ret

and StorytellerCompProperties_NeuroSama() as this =
    inherit StorytellerCompProperties()
    do this.compClass <- typeof<StorytellerComp_NeuroSama>

and [<Verse.StaticConstructorOnStartup>] Game() as this =
    inherit Game<Actions>()

    let cts = new Threading.CancellationTokenSource()
    let mutable harmony = Harmony("org.pavluk.neurator")

    // initialize instance
    static do Game.Instance |> ignore

    do
        try
            harmony.PatchAll(Assembly.GetExecutingAssembly())
            let cnt = Seq.fold (fun x _ -> x + 1) 0 (harmony.GetPatchedMethods())
            this.Start(None, cts.Token) |> ignore
            this.LogDebug(sprintf "Plugin Neurator is loaded with %d patches!" cnt)
        with exc ->
            this.LogError(sprintf "ERROR %A" exc)

    static member Instance: Game = Game()

    member _.EatRandyIncidents(_incidents: FiringIncident seq) = ()

    override _.ReregisterActions() = ()
    override _.Name = "RimWorld"

    override _.HandleAction(action: Actions) =
        match action with
        | Test _countryName -> Error None

    override _.LogError error =
        Printf.ksprintf Verse.Log.Error "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error
        eprintfn "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error

    override _.LogDebug error =
        Printf.ksprintf Verse.Log.Warning "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error
        eprintfn "%A.%s %s" DateTime.UtcNow (DateTime.UtcNow.ToString("fff")) error

    member _.Update() = ()
