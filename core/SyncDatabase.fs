namespace JoyReactor
open Types

module CofxStorage =
    open Types

    type LocalDb =
        { feeds : Map<Source, PostsWithLevels>
          posts : Map<int, Post>
          tags : Tag []
          messages : Message []
          profile : Profile option
          parseRequests : string Set }

module DiffActions =
    open Types

    type PostsActions =
        | Add of int * Post
        | Remove of int

    type ParseRequestsActions =
        | Add of string
        | Remove of int

    type AllDiffActions =
        | Posts of PostsActions
        | ParseRequests of ParseRequestsActions

module SyncStore =
    open DiffActions
    open CofxStorage
    type Enc = System.Text.Encoding
    type Uri = System.Uri

    let toJsonString : (obj -> string) ref = ref (fun _ -> failwith "not implemented")
    let fromJsonString : (string -> obj) ref = ref (fun _ -> failwith "not implemented")

    type Diff = Diff of byte []
    type KVDiff = (string * string) list

    let emptyDb = { feeds = Map.empty; posts = Map.empty; tags = [||]; messages = [||]; profile = None; parseRequests = Set.empty }

    let shared = ref emptyDb

    let serialize (actions : AllDiffActions list) : KVDiff = 
        actions
        |> List.map (
            function
            | Posts (PostsActions.Add (id, post)) -> "p_add", post |> box |> !toJsonString
            | Posts (PostsActions.Remove id) -> "p_remove", string id
            | ParseRequests (Add x) -> "pr_add", x
            | ParseRequests (Remove x) -> "pr_remove", string x)

    let deserialize (form : KVDiff) : AllDiffActions list =
        form
        |> List.map (
            function
            | "p_add", x -> x |> !fromJsonString |> unbox<Post> |> (fun p -> PostsActions.Add(p.id, p)) |> Posts
            | "p_remove", id -> int id |> PostsActions.Remove |> Posts
            | "pr_add", x -> x |> ParseRequestsActions.Add |> ParseRequests
            | "pr_remove", x -> int x |> ParseRequestsActions.Remove |> ParseRequests
            | k, _ -> failwith k)

    let getDiff (old : LocalDb) (newDb : LocalDb) = 
        let added = 
            Set.difference newDb.parseRequests old.parseRequests 
            |> Set.toList
            |> List.map (fun x -> ParseRequestsActions.Add x)
        let removed = 
            Set.difference old.parseRequests newDb.parseRequests
            |> Set.toList
            |> List.map (fun x -> ParseRequestsActions.Remove <| x.GetHashCode())
        let prActions = List.concat [ added; removed ]
        let actions = prActions |> List.map (fun x -> AllDiffActions.ParseRequests x)
        serialize actions
    
    let getDiffForAll (old : LocalDb) (newDb : LocalDb) = 
        List.concat [ 
            Set.difference 
                (old.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
                (newDb.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
            |> Seq.map (fun id -> PostsActions.Remove id |> AllDiffActions.Posts)
            |> Seq.toList;
            Set.difference 
                (newDb.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
                (old.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
            |> Seq.map (fun id -> PostsActions.Add (id, newDb.posts.[id]) |> AllDiffActions.Posts)
            |> Seq.toList;
            Set.difference newDb.parseRequests old.parseRequests |> Set.toList
            |> List.map (fun x -> ParseRequestsActions.Add x |> AllDiffActions.ParseRequests);
            Set.difference old.parseRequests newDb.parseRequests |> Set.toList
            |> List.map (fun x -> x.GetHashCode() |> ParseRequestsActions.Remove |> AllDiffActions.ParseRequests)
        ] |> serialize

    let applyDiff (db : LocalDb) (kv : KVDiff) : LocalDb =
        deserialize kv
        |> List.fold
               (fun db action ->
                    match action with
                    | Posts (PostsActions.Add (id, p)) ->
                        { db with posts = Map.add id p db.posts }
                    | Posts (PostsActions.Remove id) ->
                        { db with posts = Map.remove id db.posts }
                    | ParseRequests (ParseRequestsActions.Add x) ->
                        { db with parseRequests = Set.add x db.parseRequests }
                    | ParseRequests (ParseRequestsActions.Remove hash) ->
                        { db with parseRequests = db.parseRequests |> Set.filter (fun x -> x.GetHashCode() <> hash) })
               db

    let private callback : (LocalDb -> unit) option ref = ref None

    let sendToServer : (KVDiff -> KVDiff Async) ref = ref (fun _ -> failwith "Not implemented")

    let update (f : LocalDb -> LocalDb * 'a) : 'a Async = async {
        let oldDb = !shared
        let (newDb, x) = f oldDb
        printfn "LOGX (1.1)"
        if newDb <> oldDb then
            printfn "LOGX (1.2)"
            if newDb.parseRequests <> oldDb.parseRequests then
                printfn "LOGX (1.3)"
                let diff = getDiff oldDb newDb
                let! serverResponseDiff = !sendToServer diff
                printfn "LOGX (1.3.1) | %O" serverResponseDiff
                let newDb2 = applyDiff newDb serverResponseDiff
                shared := newDb2
            else
                printfn "LOGX (1.4)"
                shared := newDb 
            printfn "LOGX (1.5)"
            match !callback with Some f -> f !shared | _ -> () 
        printfn "LOGX (1.6)"
        return x }

    let listenUpdates (f : LocalDb -> LocalDb) (diffFromClient : KVDiff) = 
        let db = applyDiff emptyDb diffFromClient
        let updatedDb = f db
        let responseKv = getDiffForAll db updatedDb
        responseKv
        |> List.map (fun (k, v) -> sprintf "%s=%s" (Uri.EscapeDataString k) (Uri.EscapeDataString v))
        |> function
           | [] -> ""
           | x :: [] -> x
           | xs -> xs |> List.reduce (sprintf "%s&%s")
        |> Enc.UTF8.GetBytes
        |> Diff

    let sub =
        [ fun dispatch ->
              callback := Some dispatch
              dispatch !shared ]

    let dispatch f = update (fun db -> f db, ())
