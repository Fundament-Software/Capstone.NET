namespace Fundament.Capstone.Compiler

/// Defines an aribrary multi-way tree
/// This is an eager rose tree, so unlike Haskell's Data.Tree, this cannot be infinite.
type RoseTree<'T> = { Root: 'T; Children: RoseForest<'T> }

and RoseForest<'T> = RoseTree<'T> list

module RoseTree =

    let leaf root = { Root = root; Children = [] }

    let fromEdges (edges: Map<'a, list<'a>>) (rootNodes: 'a list) : RoseForest<'a> =
        // Basically a DFS, but on the return step we construct the RoseTree
        // Implemented the DFS imperatively because the optimal implementation is easier to read and write than
        // a tail-recursive DFS.
        let mutable stack = rootNodes
        let mutable visited = Set.empty
        let mutable acc = Map.empty

        while not stack.IsEmpty do
            if not (Set.contains stack.Head visited) then
                // New node, mark it as visited
                visited <- Set.add stack.Head visited

                // If it has children, add it to the stack
                match Map.tryFind stack.Head edges with
                | Some children -> stack <- children @ stack
                | None -> ()
            else
                // We're returning to this node after previously visiting, meaning all children (if any) have been visited and processed.
                let children =
                    edges
                    |> Map.tryFind stack.Head
                    |> Option.defaultValue []
                    |> List.map (fun child -> Map.find child acc)

                let node =
                    { Root = stack.Head
                      Children = children }

                acc <- Map.add stack.Head node acc
                stack <- stack.Tail

        rootNodes |> List.map (fun root -> Map.find root acc)

    let rec foldTree<'T, 'State> (folder: 'T -> 'State list -> 'State) (tree: RoseTree<'T>) =
        match tree with
        | { Root = root; Children = [] } -> folder root []
        | { Root = root; Children = children } -> folder root (List.map (foldTree folder) children)
