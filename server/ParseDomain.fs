module ParseDomain

module S = JoyReactor.SyncStore
module P = JoyReactor.Parsers
type Store = JoyReactor.CofxStorage.LocalDb

let parse html (db: Store) =
    let messages = P.getMessages html
    { db with
         sharedMessages = messages |> (Option.map ^ fun x -> x.messages |> Set.ofSeq) |> Option.defaultValue Set.empty
         sharedFeeds = P.parsePostsWithNext html |> Some
         nextMessagesPage = messages |> Option.bind ^ fun x -> x.nextPage
         userName = P.parseUserName html
         userTags = html |> P.readUserTags |> Seq.map (fun x -> x.name, x) |> Map.ofSeq
         topTags = html |> P.parseTopTags |> Seq.map (fun x -> x.name, x) |> Map.ofSeq
         posts = P.parsePost html |> Option.fold (fun ps p -> Map.add p.id p ps) db.posts }

let handleDbChange (db: Store) =
    db.parseRequests
    |> Set.fold (fun db html -> parse html db) { db with parseRequests = Set.empty }

let sync = S.listenUpdates ^ handleDbChange
