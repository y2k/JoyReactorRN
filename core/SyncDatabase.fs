namespace JoyReactor

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

    let toJsonString : (obj -> string) ref = ref (fun _ -> failwith "not implemented")
    // let fromJsonString : (string ->)

    type Diff = Diff of byte []
    type KVDiff = (string * string) list

    let emptyDb = { feeds = Map.empty; posts = Map.empty; tags = [||]; messages = [||]; profile = None; parseRequests = Set.empty }

    let shared = ref emptyDb

    let serialize (actions : AllDiffActions list) : KVDiff = 
        actions
        |> List.map (
            function
            | Posts (PostsActions.Add (id, post)) -> "p.add", post |> box |> !toJsonString
            | Posts (PostsActions.Remove id) -> "p.remove", string id
            | ParseRequests (Add x) -> "pr.add", x
            | ParseRequests (Remove x) -> "pr.remove", string x)

    let deserialize (form : KVDiff) : AllDiffActions list =
        form
        |> List.map (
            function
            | "p.add", _ -> failwith "???"
            | "p.remove", id -> int id |> PostsActions.Remove |> Posts
            | "pr.add", x -> x |> ParseRequestsActions.Add |> ParseRequests
            | "pr.remove", x -> int x |> ParseRequestsActions.Remove |> ParseRequests
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

    let applyDiff (db : LocalDb) (_ : KVDiff) : LocalDb = failwith "???"
    let sendToServer (_ : KVDiff) : KVDiff Async = failwith "???"

    let private callback : (LocalDb -> unit) option ref = ref None

    let update (f : LocalDb -> LocalDb * 'a) : 'a Async = async {
        let oldDb = !shared
        let (newDb, x) = f oldDb
        if newDb <> oldDb then
            if newDb.parseRequests <> oldDb.parseRequests then
                let diff = getDiff oldDb newDb
                let! serverResponseDiff = sendToServer(diff)
                let newDb2 = applyDiff newDb serverResponseDiff
                shared := newDb2
            else
                shared := newDb 
            match !callback with Some f -> f !shared | _ -> () 
        return x }

    let listenUpdates (f : LocalDb -> LocalDb) (diffFromClient : KVDiff) = 
        let db = applyDiff emptyDb diffFromClient
        let updatedDb = f db
        getDiffForAll db updatedDb

    let sub =
        [ fun dispatch ->
              callback := Some dispatch
              dispatch !shared ]

    let dispatch f = update (fun db -> f db, ())
