namespace Neurator

open System.Collections
open HarmonyLib
open RimWorld

[<HarmonyPatch>]
type public Patches() =
    [<HarmonyPatch(typeof<StorytellerComp_RandomMain>,
                   nameof Unchecked.defaultof<StorytellerComp_RandomMain>.MakeIntervalIncidents)>]
    [<HarmonyPostfix>]
    static member public StopRandy
        (
            __result: Generic.IEnumerable<FiringIncident> byref,
            __instance: StorytellerComp_RandomMain,
            target: IIncidentTarget
        ) =
        match target with
        | :? Verse.Map as map when Seq.contains IncidentTargetTagDefOf.Map_PlayerHome (map.IncidentTargetTags()) ->
            let neuro =
                Verse.Find.Storyteller.storytellerComps
                |> Seq.tryPick (function
                    | :? StorytellerComp_NeuroSama as x -> Some x
                    | _ -> None)

            match neuro with
            | Some neuro ->
                try
                    neuro.EatRandyIncidents __result target
                with exc ->
                    Game.Instance.LogError(exc.ToString())
            | None -> __result <- Seq.empty
        | _ -> ()

    [<HarmonyPatch(typeof<Verse.TickManager>, nameof Unchecked.defaultof<Verse.TickManager>.DoSingleTick)>]
    [<HarmonyPostfix>]
    static member public Update() =
        if Game.Instance <> null then
            try
                Game.Instance.Update()
            with exc ->
                Game.Instance.LogError(exc.ToString())
