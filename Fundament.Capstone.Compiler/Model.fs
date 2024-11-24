namespace Fundament.Capstone.Compiler.Model

open Capnp.Schema
open FSharpPlus
open Fundament.Capstone.Compiler.Useful
open System.Runtime.CompilerServices
open Fundament.Capstone.Compiler
open SpectreCoff
open Spectre.Console


type Id = uint64

type ElementSize =
    | Empty = 0
    | Bit = 1
    | Byte = 2
    | TwoBytes = 3
    | FourBytes = 4
    | EightBytes = 5
    | Pointer = 6
    | InlineComposite = 7

type Value =
    | Void
    | Bool of bool
    | Int8 of int8
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Float32 of float32
    | Float64 of float
    | Text of string
    | Data of byte array
    | List of obj
    | Enum of uint16
    | Struct of obj
    | Interface
    | AnyPointer of obj

    static member Read(reader: Capnp.Schema.Value.READER) =
        match reader.which with
        | Value.WHICH.Void -> Void
        | Value.WHICH.Bool -> Bool(reader.Bool)
        | Value.WHICH.Int8 -> Int8(reader.Int8)
        | Value.WHICH.Int16 -> Int16(reader.Int16)
        | Value.WHICH.Int32 -> Int32(reader.Int32)
        | Value.WHICH.Int64 -> Int64(reader.Int64)
        | Value.WHICH.Uint8 -> UInt8(reader.Uint8)
        | Value.WHICH.Uint16 -> UInt16(reader.Uint16)
        | Value.WHICH.Uint32 -> UInt32(reader.Uint32)
        | Value.WHICH.Uint64 -> UInt64(reader.Uint64)
        | Value.WHICH.Float32 -> Float32(reader.Float32)
        | Value.WHICH.Float64 -> Float64(reader.Float64)
        | Value.WHICH.Text -> Text(reader.Text)
        | Value.WHICH.Data -> Data(reader.Data |> Array.ofSeq)
        | Value.WHICH.List -> List(reader.List)
        | Value.WHICH.Enum -> Enum(reader.Enum)
        | Value.WHICH.Struct -> BLOCKED_ON [ "I don't know what to do with structs yet" ]
        | Value.WHICH.Interface -> Interface
        | Value.WHICH.AnyPointer -> BLOCKED_ON [ "I don't know what to do with any pointers yet" ]
        | x -> outOfRange (int32 x)

type Type =
    | Void
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | Float32
    | Float64
    | Text
    | Data
    | List of Type
    | Enum of TypeId: Id * Brand: Brand
    | Struct of TypeId: Id * Brand: Brand
    | Interface of TypeId: Id * Brand: Brand
    | AnyPointer of AnyPointerType

    static let rec ReadImpl (reader: Capnp.Schema.Type.READER) =
        match reader.which with
        | Type.WHICH.Void -> Void
        | Type.WHICH.Bool -> Bool
        | Type.WHICH.Int8 -> Int8
        | Type.WHICH.Int16 -> Int16
        | Type.WHICH.Int32 -> Int32
        | Type.WHICH.Int64 -> Int64
        | Type.WHICH.Uint8 -> UInt8
        | Type.WHICH.Uint16 -> UInt16
        | Type.WHICH.Uint32 -> UInt32
        | Type.WHICH.Uint64 -> UInt64
        | Type.WHICH.Float32 -> Float32
        | Type.WHICH.Float64 -> Float64
        | Type.WHICH.Text -> Text
        | Type.WHICH.Data -> Data
        | Type.WHICH.List -> List(ReadImpl(reader.List.ElementType))
        | Type.WHICH.Enum -> Enum(reader.Enum.TypeId, Brand.Read(reader.Enum.Brand))
        | Type.WHICH.Struct -> Struct(reader.Struct.TypeId, Brand.Read(reader.Struct.Brand))
        | Type.WHICH.Interface -> Interface(reader.Interface.TypeId, Brand.Read(reader.Interface.Brand))
        | Type.WHICH.AnyPointer -> AnyPointer(AnyPointerType.Read(reader.AnyPointer))
        | x -> outOfRange (int32 x)

    static member Read(reader: Capnp.Schema.Type.READER) = ReadImpl reader

/// Corresponds to the "anyPointer" union variant in Type in the schema.
/// "anyPointer" is itself a union, which contains an "unconstrained" union variant that we've flattened here.
and AnyPointerType =
    | UnconstrainedAnyKind
    | UnconstrainedStruct
    | UnconstrainedList
    | UnconstrainedCapability
    | Parameter of Id: Id * ParameterIndex: uint16
    | ImplicitMethodParameter of ParameterIndex: uint16

    static member Read(reader: Capnp.Schema.Type.anyPointer.READER) =
        match reader.which with
        | Type.anyPointer.WHICH.Unconstrained ->
            match reader.Unconstrained.which with
            | Type.anyPointer.unconstrained.WHICH.AnyKind -> UnconstrainedAnyKind
            | Type.anyPointer.unconstrained.WHICH.Struct -> UnconstrainedStruct
            | Type.anyPointer.unconstrained.WHICH.List -> UnconstrainedList
            | Type.anyPointer.unconstrained.WHICH.Capability -> UnconstrainedCapability
            | x -> outOfRange (int32 x)
        | Type.anyPointer.WHICH.Parameter -> Parameter(reader.Parameter.ScopeId, reader.Parameter.ParameterIndex)
        | Type.anyPointer.WHICH.ImplicitMethodParameter ->
            ImplicitMethodParameter(reader.ImplicitMethodParameter.ParameterIndex)
        | x -> outOfRange (int32 x)

// `and` is used here because BrandBinding depends on Type and Type depends on Brand.
and Brand =
    { Scopes: BrandScope list }

    static member Read(reader: Capnp.Schema.Brand.READER) =
        { Scopes = readList reader.Scopes BrandScope.Read }

/// Corresponds to "Brand.Scope" in the schema
and BrandScope =
    { ScopeId: Id
      Variant: BrandScopeVariant }

    static member Read(reader: Capnp.Schema.Brand.Scope.READER) =
        { ScopeId = reader.ScopeId
          Variant = BrandScopeVariant.Read reader }

/// Corresponds to the union inside "Brand.Scope" in the schema
and BrandScopeVariant =
    | Bind of BrandBinding list
    | Inherit

    static member Read(reader: Capnp.Schema.Brand.Scope.READER) =
        match reader.which with
        | Brand.Scope.WHICH.Bind -> Bind(readList reader.Bind BrandBinding.Read)
        | Brand.Scope.WHICH.Inherit -> Inherit
        | x -> outOfRange (int32 x)

/// Corresponds to "Brand.Binding" in the schema
and BrandBinding =
    | Unbound
    | Type of Type

    static member Read(reader: Capnp.Schema.Brand.Binding.READER) =
        match reader.which with
        | Brand.Binding.WHICH.Unbound -> Unbound
        | Brand.Binding.WHICH.Type -> Type(Type.Read reader.Type)
        | x -> outOfRange (int32 x)

type Annotation =
    { Id: Id
      Brand: Brand
      Value: Value }

    static member Read(reader: Capnp.Schema.Annotation.READER) =
        { Id = reader.Id
          Brand = Brand.Read reader.Brand
          Value = Value.Read reader.Value }

type Field =
    { Name: string
      CodeOrder: uint16
      Annotations: Annotation list
      DiscriminantValue: uint16
      Variant: FieldVariant
      Ordinal: FieldOrdinal }

    static member Read(reader: Capnp.Schema.Field.READER) =
        let annotations = readList reader.Annotations Annotation.Read
        let variant = FieldVariant.Read reader
        let ordinal = FieldOrdinal.Read reader.Ordinal

        { Name = reader.Name
          CodeOrder = reader.CodeOrder
          Annotations = annotations
          DiscriminantValue = reader.DiscriminantValue
          Variant = variant
          Ordinal = ordinal }

/// Corresponds to the inner union of "Field" in the schema
and FieldVariant =
    | Slot of Offset: uint32 * Type: Type * DefaultValue: Value * HadExplicitDefault: bool
    | Group of TypeId: Id

    static member Read(reader: Capnp.Schema.Field.READER) =
        match reader.which with
        | Field.WHICH.Slot ->
            Slot(
                reader.Slot.Offset,
                Type.Read reader.Slot.Type,
                Value.Read reader.Slot.DefaultValue,
                reader.Slot.HadExplicitDefault
            )
        | Field.WHICH.Group -> Group(reader.Group.TypeId)
        | x -> outOfRange (int32 x)

/// Corresponds to "Field.ordinal" in the schema
and FieldOrdinal =
    | Implicit
    | Explicit of uint16

    static member Read(reader: Capnp.Schema.Field.ordinal.READER) =
        match reader.which with
        | Field.ordinal.WHICH.Implicit -> Implicit
        | Field.ordinal.WHICH.Explicit -> Explicit(reader.Explicit)
        | x -> outOfRange (int32 x)

type Enumerant =
    { Name: string
      CodeOrder: uint16
      Annotations: Annotation list }

    static member Read(reader: Capnp.Schema.Enumerant.READER) =
        { Name = reader.Name
          CodeOrder = reader.CodeOrder
          Annotations = readList reader.Annotations Annotation.Read }

type Superclass =
    { Id: Id
      Brand: Brand }

    static member Read(reader: Capnp.Schema.Superclass.READER) =
        { Id = reader.Id
          Brand = Brand.Read reader.Brand }

type Method =
    { Name: string
      CodeOrder: uint16
      ImplicitParameters: string list
      ParamStructType: Id
      ParamBrand: Brand
      ResultStructType: Id
      ResultBrand: Brand
      Annotations: Annotation list }

    static member Read(reader: Capnp.Schema.Method.READER) =
        let implicitParameters =
            reader.ImplicitParameters |> Seq.map (fun reader -> reader.Name) |> List.ofSeq

        let annotations = readList reader.Annotations Annotation.Read

        { Name = reader.Name
          CodeOrder = reader.CodeOrder
          ImplicitParameters = implicitParameters
          ParamStructType = reader.ParamStructType
          ParamBrand = Brand.Read reader.ParamBrand
          ResultStructType = reader.ResultStructType
          ResultBrand = Brand.Read reader.ResultBrand
          Annotations = annotations }

type Node =
    { Id: Id
      SymbolName: string option
      DisplayName: string
      DisplayNamePrefixLength: uint32
      Parameters: string list
      IsGeneric: bool
      Annotations: Annotation list
      Variant: NodeVariant }

    static member Read nameTable (reader: Capnp.Schema.Node.READER) =
        { Id = reader.Id
          SymbolName = Map.tryFind reader.Id nameTable
          DisplayName = reader.DisplayName
          DisplayNamePrefixLength = reader.DisplayNamePrefixLength
          Parameters = readList reader.Parameters (fun reader -> reader.Name)
          IsGeneric = reader.IsGeneric
          Annotations = readList reader.Annotations Annotation.Read
          Variant = NodeVariant.Read reader }

// Exists so we can pattern match on the variant without having to repeat the reader type names
and private NodeVariantReader =
    | File
    | Struct of Capnp.Schema.Node.``struct``.READER
    | Enum of Capnp.Schema.Node.``enum``.READER
    | Interface of Capnp.Schema.Node.``interface``.READER
    | Const of Capnp.Schema.Node.``const``.READER
    | Annotation of Capnp.Schema.Node.``annotation``.READER

    static member FromReader(reader: Capnp.Schema.Node.READER) =
        match reader.which with
        | Node.WHICH.File -> File
        | Node.WHICH.Struct -> Struct(reader.Struct)
        | Node.WHICH.Enum -> Enum(reader.Enum)
        | Node.WHICH.Interface -> Interface(reader.Interface)
        | Node.WHICH.Const -> Const(reader.Const)
        | Node.WHICH.Annotation -> Annotation(reader.Annotation)
        | x -> outOfRange (int32 x)

and NodeVariant =
    | File
    | Struct of
        DataWordCount: uint16 *
        PointerCount: uint16 *
        IsGroup: bool *
        DiscriminantCount: uint16 *
        DiscriminantOffset: uint32 *
        Fields: Field list
    | Enum of Enumerant list
    | Interface of Methods: Method list * Superclasses: Superclass list
    | Const of Type: Type * Value: Value
    | Annotation of
        Type: Type *
        // TODO: Can an annotation target multiple things at once? Or can this be compressed into an enum in the object model?
        TargetsFile: bool *
        TargetsConst: bool *
        TargetsEnum: bool *
        TargetsEnumerant: bool *
        TargetsStruct: bool *
        TargetsField: bool *
        TargetsUnion: bool *
        TargetsGroup: bool *
        TargetsInterface: bool *
        TargetsMethod: bool *
        TargetsParam: bool *
        TargetsAnnotation: bool

    static member Read(reader: Capnp.Schema.Node.READER) =
        match NodeVariantReader.FromReader reader with
        | NodeVariantReader.File -> File
        | NodeVariantReader.Struct(structReader) ->
            Struct(
                structReader.DataWordCount,
                structReader.PointerCount,
                structReader.IsGroup,
                structReader.DiscriminantCount,
                structReader.DiscriminantOffset,
                []
            )
        | NodeVariantReader.Enum(enumReader) -> Enum(readList enumReader.Enumerants Enumerant.Read)
        | NodeVariantReader.Interface(interfaceReader) ->
            Interface(
                readList interfaceReader.Methods Method.Read,
                readList interfaceReader.Superclasses Superclass.Read
            )
        | NodeVariantReader.Const(constReader) -> Const(Type.Read constReader.Type, Value.Read constReader.Value)
        | NodeVariantReader.Annotation(annotationReader) ->
            Annotation(
                Type.Read annotationReader.Type,
                annotationReader.TargetsFile,
                annotationReader.TargetsConst,
                annotationReader.TargetsEnum,
                annotationReader.TargetsEnumerant,
                annotationReader.TargetsStruct,
                annotationReader.TargetsField,
                annotationReader.TargetsUnion,
                annotationReader.TargetsGroup,
                annotationReader.TargetsInterface,
                annotationReader.TargetsMethod,
                annotationReader.TargetsParam,
                annotationReader.TargetsAnnotation
            )


[<Extension>]
type CodeGeneratorRequestExtensions =
    [<Extension>]
    static member BuildModel(reader: CodeGeneratorRequest.READER) : RoseForest<Node> =
        // Answers the question "What is the name of the node with this Id"
        let nameTable =
            reader.Nodes
            |> Seq.collect (fun nodeReader -> nodeReader.NestedNodes)
            |> fold (fun table nnr -> Map.add nnr.Id nnr.Name table) Map.empty

        let nodeTable = reader.Nodes |> Seq.map (fun reader -> reader.Id, reader) |> Map

        let rootNodes =
            reader.Nodes |> Seq.filter (fun reader -> reader.ScopeId = 0UL) |> List.ofSeq

        // Answers the question "What are the children of the node with this Id"
        let childrenTable =
            let foldFn table (reader: Capnp.Schema.Node.READER) =
                let parentId = reader.ScopeId

                let childrenIds =
                    match Map.tryFind parentId table with
                    | None -> [ reader.Id ]
                    | Some(childrenIds) -> reader.Id :: childrenIds

                Map.add parentId childrenIds table

            Seq.fold foldFn Map.empty reader.Nodes

        // Recursively builds a tree of nodes rooted at the given node reader
        let rec buildTree (reader: Capnp.Schema.Node.READER) =
            { Root = Node.Read nameTable reader
              Children =
                childrenTable
                |> Map.tryFind reader.Id
                |> Option.defaultValue []
                |> List.map (fun childId -> nodeTable[childId])
                |> List.map buildTree }

        rootNodes |> List.map buildTree
