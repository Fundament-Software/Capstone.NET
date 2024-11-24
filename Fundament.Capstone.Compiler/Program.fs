open Argu
open System
open System.Reflection
open Capnp
open Capnp.Schema
open SpectreCoff
open Fundament.Capstone.Compiler.Model
open Fundament.Capstone.Compiler
open Spectre.Console

type Arguments =
    | [<First; Unique>] File of string
    | Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version -> "Display the version of the compiler."
            | File _ -> "Read a binary-encoded code generation request from the file at the specified path."

let inputStreamResult =
    let VersionLine =
        $"Capstone C# Compiler Plugin Version {Assembly.GetExecutingAssembly().GetName().Version}"

    let HelpTextMessage =
        $"{VersionLine}\n\
        Normally invoked by the Capstone compiler, this program reads a binary-encoded code generation request from a file or stdin if no file is provided."

    let parser = ArgumentParser.Create<Arguments>()

    let parsedArguments = parser.ParseCommandLine(raiseOnUsage = false)

    if parsedArguments.IsUsageRequested then
        do printfn "%s" (parser.PrintUsage(HelpTextMessage))
        Error 1
    elif parsedArguments.Contains(Version) then
        do printfn "%s" VersionLine
        Error 1
    else
        let s =
            match parsedArguments.TryGetResult(File) with
            | None ->
                if not Console.IsInputRedirected then
                    do printfn $"{VersionLine}\nExpecting binary-encoded code generation request from stdin..."

                Console.OpenStandardInput()
            | Some(path) -> IO.File.OpenRead path

        Ok(s)

let inputStream =
    match inputStreamResult with
    | Ok(inputStream) -> inputStream
    | Error(code) -> exit code

let model =
    inputStream
    |> Framing.ReadSegments
    |> DeserializerState.CreateRoot
    |> CodeGeneratorRequest.READER.create
    |> CodeGeneratorRequestExtensions.BuildModel

let displayTree =
    let nodePrint =
        function
        | { Variant = NodeVariant.File } as n ->
            Many
                [ E "File Node "
                  MCD(Color.SteelBlue, [ Decoration.Bold ], n.DisplayName)
                  C $" Id: {n.Id}" ]
        | { Variant = NodeVariant.Struct(dwc, pc, _, _, _, fields) } as n ->
            Many
                [ E "Struct Node"
                  MCD(Color.SteelBlue, [ Decoration.Bold ], Option.defaultValue n.DisplayName n.SymbolName)
                  C $" Id: {n.Id}"
                  P $"(Data Word Count: {dwc}, Pointer Count: {pc}, Field Count: {fields.Length})" ]
        | { Variant = NodeVariant.Enum(enumerants) } as n ->
            Many
                [ E "Enum Node"
                  MCD(Color.SteelBlue, [ Decoration.Bold ], Option.defaultValue n.DisplayName n.SymbolName)
                  C $" Id: {n.Id}"
                  NL
                  BI(List.map (fun (e: Enumerant) -> P $"{e.Name} {e.CodeOrder}") enumerants) ]
        | { Variant = NodeVariant.Const(t, value) } as n ->
            Many
                [ E "Const Node"
                  MCD(Color.SteelBlue, [ Decoration.Bold ], Option.defaultValue n.DisplayName n.SymbolName)
                  C $" Id: {n.Id}"
                  P $"(Type: %A{t}, Value: %A{value})" ]
        | n -> Calm $"Node: %A{n}"

    let folder (n: Node) =
        function
        | [] -> node (nodePrint n) []
        | childNodes -> node (nodePrint n) childNodes

    List.map (fun tree -> RoseTree.foldTree folder tree) model

tree (Pumped "Compilation Request") displayTree |> toOutputPayload |> toConsole
