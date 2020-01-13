namespace JoyReactor

module CofxStorage =
    type LocalDb = JoyReactor.Database.Gen.LocalDb
    //     { feeds : Map<Source, PostsWithLevels>
    //       sharedFeeds : PostResponse option
    //       posts : Map<int, Post>
    //       userName : string option
    //       userTags : Map<string, Tag>
    //       topTags : Map<string, Tag>
    //       messages : Message Set
    //       sharedMessages : Message Set
    //       nextMessagesPage : string option
    //       profile : Profile option
    //       parseRequests : string Set }
    // with
    //     static member empty = { feeds = Map.empty; sharedFeeds = None; posts = Map.empty; userName = None; userTags = Map.empty; topTags = Map.empty; messages = Set.empty; sharedMessages = Set.empty; nextMessagesPage = None; profile = None; parseRequests = Set.empty }

// module DiffActions =
//     open Types
    
//     type UserNameActions = Changed of string option

//     type UserTagsActions =
//         | Add of string * Tag
//         | Remove of string

//     type TopTagsActions =
//         | Add of string * Tag
//         | Remove of string

//     type PostsActions =
//         | Add of int * Post
//         | Remove of int

//     type ParseRequestsActions =
//         | Add of string
//         | Remove of int
        
//     type NextMessagesPageActions = Changed of string option

//     type SharedMessagesActions =
//         | Add of Message
//         | Remove of Message

//     type SharedFeedsActions = Changed of PostResponse option
    
//     type AllDiffActions =
//         | NextMessagesPage of NextMessagesPageActions
//         | UserName of UserNameActions
//         | UserTags of UserTagsActions
//         | TopTags of TopTagsActions
//         | Posts of PostsActions
//         | ParseRequests of ParseRequestsActions
//         | SharedMessages of SharedMessagesActions
//         | SharedFeeds of SharedFeedsActions

module SyncStore =
    open Types
    // open DiffActions
    open CofxStorage
    type Enc = System.Text.Encoding
    type Uri = System.Uri

    let toJsonString : (obj -> string) ref = ref (fun _ -> failwith "toJsonString not implemented")
    let fromJsonString : (string -> obj) ref = ref (fun _ -> failwith "fromJsonString not implemented")

    type Diff = Diff of byte []
    type KVDiff = (string * string) list

    let emptyDb = 
        { Database.Gen.LocalDb.feeds = Map.empty 
          Database.Gen.LocalDb.sharedFeeds = Map.empty
          Database.Gen.LocalDb.posts = Map.empty
          Database.Gen.LocalDb.userName = Map.empty
          Database.Gen.LocalDb.userTags = Map.empty
          Database.Gen.LocalDb.topTags = Map.empty
          Database.Gen.LocalDb.messages = Map.empty
          Database.Gen.LocalDb.sharedMessages = Map.empty
          Database.Gen.LocalDb.nextMessagesPage = Map.empty
          Database.Gen.LocalDb.profile = Map.empty
          Database.Gen.LocalDb.parseRequests = Map.empty }

    let shared = ref emptyDb

    // let serialize (actions : AllDiffActions list) : KVDiff = 
    //     actions
    //     |> List.map (
    //         function
    //         | SharedFeeds (SharedFeedsActions.Changed x) -> "sf_a", {| value = x |} |> box |> !toJsonString 
    //         | SharedMessages (SharedMessagesActions.Add m) -> "sm_a", m |> box |> !toJsonString
    //         | SharedMessages (SharedMessagesActions.Remove m) -> "sm_r", m |> box |> !toJsonString
    //         | NextMessagesPage (NextMessagesPageActions.Changed page) -> "mp_ch", page |> Option.defaultValue ""
    //         | UserName (UserNameActions.Changed userName) -> "un_ch", userName |> Option.defaultValue ""
    //         | TopTags (TopTagsActions.Add (_, tag)) -> "tt_add", tag |> box |> !toJsonString
    //         | TopTags (TopTagsActions.Remove pos) -> "tt_remove", string pos
    //         | UserTags (UserTagsActions.Add (_, tag)) -> "ut_add", tag |> box |> !toJsonString
    //         | UserTags (UserTagsActions.Remove pos) -> "ut_remove", string pos
    //         | Posts (PostsActions.Add (_, post)) -> "p_add", post |> box |> !toJsonString
    //         | Posts (PostsActions.Remove id) -> "p_remove", string id
    //         | ParseRequests (ParseRequestsActions.Add x) -> "pr_add", x
    //         | ParseRequests (ParseRequestsActions.Remove x) -> "pr_remove", string x)

    // let deserialize (form : KVDiff) : AllDiffActions list =
    //     form
    //     |> List.map (
    //         function
    //         | "sf_a", x -> x |> !fromJsonString |> unbox<{| value: PostResponse option |}> |> fun x -> x.value |> SharedFeedsActions.Changed |> SharedFeeds
    //         | "sm_a", x -> x |> !fromJsonString |> unbox |> SharedMessagesActions.Add |> SharedMessages
    //         | "sm_r", x -> x |> !fromJsonString |> unbox |> SharedMessagesActions.Remove |> SharedMessages
    //         | "mp_ch", x -> (if String.length x = 0 then Some x else None) |> NextMessagesPageActions.Changed |> NextMessagesPage
    //         | "un_ch", x -> (if String.length x = 0 then Some x else None) |> UserNameActions.Changed |> UserName
    //         | "tt_add", x -> x |> !fromJsonString |> unbox<Tag> |> (fun p -> TopTagsActions.Add(p.name, p)) |> TopTags
    //         | "tt_remove", id -> id |> TopTagsActions.Remove |> TopTags
    //         | "ut_add", x -> x |> !fromJsonString |> unbox<Tag> |> (fun p -> UserTagsActions.Add(p.name, p)) |> UserTags
    //         | "ut_remove", id -> id |> UserTagsActions.Remove |> UserTags
    //         | "p_add", x -> x |> !fromJsonString |> unbox<Post> |> (fun p -> PostsActions.Add(p.id, p)) |> Posts
    //         | "p_remove", id -> int id |> PostsActions.Remove |> Posts
    //         | "pr_add", x -> x |> ParseRequestsActions.Add |> ParseRequests
    //         | "pr_remove", x -> int x |> ParseRequestsActions.Remove |> ParseRequests
    //         | k, _ -> failwith k)

    // let private makeStringHashId (s : string) =
    //     let mutable i = 0
    //     let mutable h = 5381
    //     let len = s.Length
    //     while (i < len) do
    //         let a = int <| s.Chars i
    //         h <- h * 33 ^^^ a
    //         i <- i + 1
    //     h
    
    // let getDiff (old : LocalDb) (newDb : LocalDb) = 
    //     let added = 
    //         Set.difference newDb.parseRequests old.parseRequests 
    //         |> Set.toList
    //         |> List.map (fun x -> ParseRequestsActions.Add x)
    //     let removed = 
    //         Set.difference old.parseRequests newDb.parseRequests
    //         |> Set.toList
    //         |> List.map (fun x -> ParseRequestsActions.Remove <| (makeStringHashId x))
    //     let prActions = List.concat [ added; removed ]
    //     let actions = prActions |> List.map (fun x -> AllDiffActions.ParseRequests x)
    //     serialize actions
    
    // let getDiffForAll (old : LocalDb) (newDb : LocalDb) = 
    //     List.concat [
    //         (if (old.sharedFeeds <> newDb.sharedFeeds)
    //             then [ newDb.sharedFeeds |> SharedFeedsActions.Changed |> AllDiffActions.SharedFeeds ]
    //             else [])

    //         Set.difference newDb.sharedMessages old.sharedMessages |> Set.toList
    //         |> List.map (fun x -> SharedMessagesActions.Add x |> AllDiffActions.SharedMessages)
    //         Set.difference old.sharedMessages newDb.sharedMessages |> Set.toList
    //         |> List.map (fun x -> SharedMessagesActions.Remove x |> AllDiffActions.SharedMessages)

    //         (if newDb.nextMessagesPage <> old.nextMessagesPage
    //             then [ newDb.nextMessagesPage |> NextMessagesPageActions.Changed |> AllDiffActions.NextMessagesPage ] 
    //             else [])

    //         (if newDb.userName <> old.userName
    //             then [ newDb.userName |> UserNameActions.Changed |> AllDiffActions.UserName ] 
    //             else [])

    //         Set.difference 
    //             (old.topTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (newDb.topTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> TopTagsActions.Remove id |> AllDiffActions.TopTags)
    //         |> Seq.toList;
    //         Set.difference 
    //             (newDb.topTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (old.topTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> TopTagsActions.Add (id, newDb.topTags.[id]) |> AllDiffActions.TopTags)
    //         |> Seq.toList;

    //         Set.difference 
    //             (old.userTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (newDb.userTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> UserTagsActions.Remove id |> AllDiffActions.UserTags)
    //         |> Seq.toList;
    //         Set.difference 
    //             (newDb.userTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (old.userTags |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> UserTagsActions.Add (id, newDb.topTags.[id]) |> AllDiffActions.UserTags)
    //         |> Seq.toList;

    //         Set.difference 
    //             (old.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (newDb.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> PostsActions.Remove id |> AllDiffActions.Posts)
    //         |> Seq.toList;
    //         Set.difference 
    //             (newDb.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //             (old.posts |> Seq.map (fun x -> x.Key) |> Set.ofSeq)
    //         |> Seq.map (fun id -> PostsActions.Add (id, newDb.posts.[id]) |> AllDiffActions.Posts)
    //         |> Seq.toList;

    //         Set.difference newDb.parseRequests old.parseRequests |> Set.toList
    //         |> List.map (fun x -> ParseRequestsActions.Add x |> AllDiffActions.ParseRequests);
    //         Set.difference old.parseRequests newDb.parseRequests |> Set.toList
    //         |> List.map (fun x -> (makeStringHashId x) |> ParseRequestsActions.Remove |> AllDiffActions.ParseRequests)
    //     ] |> serialize

    // let applyDiff (db : LocalDb) (kv : KVDiff) : LocalDb =
    //     deserialize kv
    //     |> List.fold
    //            (fun db action ->
    //                 match action with
    //                 | SharedFeeds (SharedFeedsActions.Changed x) -> { db with sharedFeeds = x }
    //                 | SharedMessages (SharedMessagesActions.Add x) -> { db with sharedMessages = db.sharedMessages |> Set.add x }
    //                 | SharedMessages (SharedMessagesActions.Remove x) -> { db with sharedMessages = db.sharedMessages |> Set.remove x }
    //                 | NextMessagesPage (NextMessagesPageActions.Changed page) -> { db with nextMessagesPage = page }
    //                 | UserName (UserNameActions.Changed userName) -> { db with userName = userName }
    //                 | UserTags (UserTagsActions.Add (id, p)) -> { db with userTags = Map.add id p db.userTags }
    //                 | UserTags (UserTagsActions.Remove id) -> { db with userTags = Map.remove id db.userTags }
    //                 | TopTags (TopTagsActions.Add (id, p)) -> { db with topTags = Map.add id p db.topTags }
    //                 | TopTags (TopTagsActions.Remove id) -> { db with topTags = Map.remove id db.topTags }
    //                 | Posts (PostsActions.Add (id, p)) -> { db with posts = Map.add id p db.posts }
    //                 | Posts (PostsActions.Remove id) -> { db with posts = Map.remove id db.posts }
    //                 | ParseRequests (ParseRequestsActions.Add x) -> { db with parseRequests = Set.add x db.parseRequests }
    //                 | ParseRequests (ParseRequestsActions.Remove hash) ->
    //                     { db with parseRequests = db.parseRequests |> Set.filter (fun x -> x.GetHashCode() <> hash) })
    //            db

    let private callback : (LocalDb -> unit) option ref = ref None

    let sendToServer : (KVDiff -> KVDiff Async) ref = ref (fun _ -> failwith "Not implemented")

    let update (f : LocalDb -> LocalDb * 'a) : 'a Async = failwith "???"
        // async {
        // let oldDb = !shared
        // let (newDb, x) = f oldDb
        // if newDb <> oldDb then
        //     if newDb.parseRequests <> oldDb.parseRequests then
        //         let diff = getDiff oldDb newDb
        //         let! serverResponseDiff = !sendToServer diff
        //         let newDb2 = applyDiff newDb serverResponseDiff
        //         shared := newDb2
        //     else
        //         shared := newDb 
        //     match !callback with Some f -> f !shared | _ -> () 
        // return x }

    let listenUpdates (f : LocalDb -> LocalDb) (diffFromClient : KVDiff) : Diff = failwith "???"
        // let db = applyDiff emptyDb diffFromClient
        // let updatedDb = f db
        // let responseKv = getDiffForAll db updatedDb
        // responseKv
        // |> List.map (fun (k, v) -> sprintf "%s=%s" (Uri.EscapeDataString k) (Uri.EscapeDataString v))
        // |> function
        //    | [] -> ""
        //    | x :: [] -> x
        //    | xs -> xs |> List.reduce (sprintf "%s&%s")
        // |> Enc.UTF8.GetBytes
        // |> Diff

    let sub =
        [ fun dispatch ->
              callback := Some dispatch
              dispatch !shared ]

    let dispatch f = update (fun db -> f db, ())
