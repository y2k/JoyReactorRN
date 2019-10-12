namespace JoyReactor

module CofxStorage =
    open Types

    type LocalDb =
        { feeds : Map<Source, Post []>
          feeds' : Map<Source, PostsWithLevels>
          posts : Map<int, Post>
          tags : Tag []
          messages : Message []
          profile : Profile option
          parseRequests : string [] }

module SyncStore =
    open CofxStorage

    type Diff = Diff of byte []

    let emptyDb = { feeds = Map.empty; feeds' = Map.empty; posts = Map.empty; tags = [||]; messages = [||]; profile = None; parseRequests = [||] }

    let shared = ref emptyDb

    let getDiff (old : LocalDb) (newDb : LocalDb) : Diff = failwith "???"
    let getDiffForAll (old : LocalDb) (newDb : LocalDb) : Diff = failwith "???"
    let applyDiff (db : LocalDb) (_ : Diff) : LocalDb = failwith "???"
    let sendToServer (_ : Diff) : Diff Async = failwith "???"

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

    let listenUpdates (f : LocalDb -> LocalDb) (diffFromClient : Diff) : Diff = 
        let db = applyDiff emptyDb diffFromClient
        let updatedDb = f db
        getDiffForAll db updatedDb

    let sub =
        [ fun dispatch ->
              callback := Some dispatch
              dispatch !shared ]

    let dispatch f = update (fun db -> f db, ())
