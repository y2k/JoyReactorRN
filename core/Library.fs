namespace JoyReactor

[<AutoOpen>]
module CommonUtils =
    let inline (^) f x = f x

    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)

    let inline always a _ = a
    let inline flip f a b = f b a
    let inline curry f a b = f (a, b)
    let inline uncurry f (a, b) = f a b

module Types =
    type Source =
        | FeedSource
        | TagSource of string
        | FavoriteSource

    type Tag =
        { name : string
          image : string }

    type Attachment =
        { url : string
          aspect : float }

    type AttachmentResource =
        { image : Attachment }

    type Comment =
        { text : string
          image : Attachment
          rating : float
          userName : string
          attachments : AttachmentResource [] }

    type Post =
        { id : int
          userName : string
          userImage : Attachment
          rating : double
          created : System.DateTime
          image : Attachment option
          attachments : AttachmentResource []
          title : string
          tags : string []
          comments : Comment [] }

    type PostResponse =
        { posts : Post list
          nextPage : int option }

    type PostsWithLevels =
        { actual : Post []
          old : Post []
          preloaded : Post []
          nextPage : int option }
        static member empty : PostsWithLevels =
            { actual = [||]
              old = [||]
              preloaded = [||]
              nextPage = None }

    type Profile =
        { userName : string
          userImage : Attachment
          rating : float
          stars : int
          progressToNewStar : float }

    type Message =
        { text : string
          date : double
          isMine : bool
          userName : string
          userImage : string }

    type MessagesWithNext =
        { messages : Message []
          nextPage : string option }

module CofxStorage =
  open Types

  type LocalDb =
      { posts : Map<Source, Post list>
        tags : Tag list
        messages : Message list
        profile : Profile option }

module UrlBuilder =
    let domain = "joyreactor.cc"
