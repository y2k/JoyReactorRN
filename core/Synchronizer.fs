namespace JoyReactor

module ResponseSaver =
    open JoyReactor.Types

    let private tryAddPost posts post =
        post
        |> Option.map ^ fun p -> Map.add p.id p posts
        |> Option.defaultValue posts
    let private addMessages messages (newMessages : MessagesWithNext option) =
        match newMessages with
        | Some mesWithNext ->
            mesWithNext.messages
            |> Array.fold (fun a x -> Map.add x.date x a) messages
        | None -> messages
    let private updateNextMessagePage nextMessagesPage newMessages =
        match newMessages with
        | None -> nextMessagesPage
        | Some x -> x.nextPage

    let saveAllParseResults db (pr : ParseResponse) =
        let toMapTag tags dbTags =
            tags
            |> Option.map ^ fun x ->
                x
                |> Array.map (fun x -> x.name, x)
                |> Map.ofArray
            |> Option.defaultValue dbTags
        { db with
            sharedFeeds = pr.posts
            userName = pr.userName |> Option.orElse db.userName
            userTags = toMapTag pr.userTags db.userTags
            topTags = toMapTag pr.topTags db.topTags
            posts = tryAddPost db.posts pr.post
            profile = pr.profile |> Option.orElse db.profile
            messages = addMessages db.messages pr.messages
            nextMessagesPage = updateNextMessagePage db.nextMessagesPage pr.messages }

module SyncExecutor =
    open SyncBuilder
    open Elmish
    open JoyReactor.Types
    type private Db = LocalDb

    let mutable downloadAndParseImpl : string -> ParseResponse Async = fun _ -> failwith "not implemented"
    let mutable postFormImpl : PostForm -> ParseResponse Async = fun _ -> failwith "not implemented"

    let private db = ref Db.empty

    let resetDb () = db := Db.empty

    let postForm form =
        let invoke =
            async {
                let! pr = postFormImpl form
                db := ResponseSaver.saveAllParseResults !db pr
                return ()
            }
        Cmd.OfAsync.either (fun _ -> invoke) () Ok Error

    let private run (furl: Db -> Db * Url option) (callback: Db -> Db * 'a) : Result<'a, exn> Cmd =
        let invoke : _ Async =
            let downloadPostsForUrl url =
                async {
                    let! pr = downloadAndParseImpl url
                    db := ResponseSaver.saveAllParseResults !db pr
                }
            async {
                let (ldb, opUrl) = furl !db
                db := ldb
                match opUrl with None -> () | Some url -> do! downloadPostsForUrl url
                let (ldb, result) = callback !db
                db := ldb
                return result
            }
#if FABLE_COMPILER
        Cmd.OfAsync.either (fun _ -> invoke) () Ok Error
#else
        Cmd.OfAsyncImmediate.either (fun _ -> invoke) () Ok Error
#endif

    let exec f p =
        run (fun db -> p.pre db, p.url db) (fun db -> let db = p.post db in db, p.convert db)
        |> Cmd.map f

    let exec' f p = exec (function Ok x -> f x | Error e -> raise e) p
