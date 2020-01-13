namespace JoyReactor

module Types =

    open SyncGenerator.Lib.Serializer

    type Source =
        | FeedSource
        | TagSource of string
        | FavoriteSource

    let SourceC =
        sum3C
            UnitC (fun _ -> FeedSource)
            StringC TagSource
            UnitC (fun _ -> FavoriteSource)
            (fun f0 f1 f2 -> function FeedSource -> f0 () | TagSource x -> f1 x | FavoriteSource -> f2 ())

    type Tag =
        { name : string
          image : string }

    let TagC = 
        record2C
            StringC (fun x -> x.name)
            StringC (fun x -> x.image)
            (fun a b -> { name = a; image = b })

    type Attachment =
        { url : string
          aspect : float }

    let AttachmentC =
        record2C
            StringC (fun x -> x.url)
            DoubleC (fun x -> x.aspect)
            (fun a b -> { url = a; aspect = b })

    type AttachmentResource =
        { image : Attachment }

    let AttachmentResourceC =
        record1C
            AttachmentC (fun x -> x.image)
            (fun a -> { image = a })

    type Comment =
        { text : string
          image : Attachment
          rating : float
          userName : string
          attachments : AttachmentResource [] }

    let CommentC =
        record5C
            StringC (fun x -> x.text)
            AttachmentC (fun x -> x.image)
            DoubleC (fun x -> x.rating)
            StringC (fun x -> x.userName)
            (ArrayC AttachmentResourceC) (fun x -> x.attachments)
            (fun a0 a1 a2 a3 a4 -> { text = a0; image = a1; rating = a2; userName = a3; attachments = a4 })

    type Post =
        { id : int
          userName : string
          userImage : Attachment
          rating : double
          created : System.DateTime
          image : Attachment []
          attachments : AttachmentResource []
          title : string
          tags : string []
          comments : Comment [] }

    let PostC : Post C =
      record10C
          IntC (fun x -> x.id)
          StringC (fun x -> x.userName)
          AttachmentC (fun x -> x.userImage)
          DoubleC (fun x -> x.rating)
          DateTimeC (fun x -> x.created)
          (ArrayC AttachmentC) (fun x -> x.image)
          (ArrayC AttachmentResourceC) (fun x -> x.attachments)
          StringC (fun x -> x.title)
          (ArrayC StringC) (fun x -> x.tags)
          (ArrayC CommentC) (fun x -> x.comments)
          (fun a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 ->
              { id = a0; userName = a1; userImage = a2; rating = a3; created = a4; image = a5; attachments = a6; title = a7; tags = a8; comments = a9 } )
    
    type PostResponse =
        { posts : Post []
          nextPage : int option }
        static member empty : PostResponse = { posts = [||]; nextPage = None }
    
    let PostResponseC =
        record2C
            (ArrayC PostC) (fun x -> x.posts)
            (OptionC IntC) (fun x -> x.nextPage)
            (fun a b -> { posts = a; nextPage = b })        

    type PostsWithLevels =
        { actual : Post []
          old : Post []
          preloaded : Post []
          nextPage : int option }
        static member empty : PostsWithLevels =
            { actual = [||]; old = [||]; preloaded = [||]; nextPage = None }

    let PostsWithLevelsC =
        record4C
            (ArrayC PostC) (fun x -> x.actual)
            (ArrayC PostC) (fun x -> x.old)
            (ArrayC PostC) (fun x -> x.preloaded)
            (OptionC IntC) (fun x -> x.nextPage)
            (fun a b c d -> { actual = a; old = b; preloaded = c; nextPage = d })

    type Profile =
        { userName : string
          userImage : Attachment
          rating : float
          stars : int
          progressToNewStar : float }

    let ProfileC =
        record5C
            StringC (fun x -> x.userName)
            AttachmentC (fun x -> x.userImage)
            DoubleC (fun x -> x.rating)
            IntC (fun x -> x.stars)
            DoubleC (fun x -> x.progressToNewStar)
            (fun a b c d e -> { userName = a; userImage = b; rating = c; stars = d; progressToNewStar = e })

    type Message =
        { text : string
          date : double
          isMine : bool
          userName : string
          userImage : string }

    type MessagesWithNext =
        { messages : Message []
          nextPage : string option }

    let MessageC : Message C =
        record5C
            StringC (fun x -> x.text)
            DoubleC (fun x -> x.date)
            BoolC (fun x -> x.isMine)
            StringC (fun x -> x.userName)
            StringC (fun x -> x.userImage)
            (fun a b c d e -> { text = a; date = b; isMine = c; userName = d; userImage = e })

module UrlBuilder =
    let domain = "joyreactor.cc"
#if DEBUG
    let apiBaseUri = "http://192.168.1.59:8080"
#else
    let apiBaseUri = "https://jrs.y2k.work"
#endif
