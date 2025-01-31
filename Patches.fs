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
            try
                Game.Instance.EatRandyIncidents __result
            with _ ->
                ()

            __result <- Seq.empty
        | _ -> ()
