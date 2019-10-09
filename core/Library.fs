namespace JoyReactor

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
            { actual = [||]; old = [||]; preloaded = [||]; nextPage = None }

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
      { feeds : Map<Source, Post []>
        feeds' : Map<Source, PostsWithLevels>
        posts : Map<int, Post>
        tags : Tag []
        messages : Message []
        profile : Profile option }

module UrlBuilder =
    let domain = "joyreactor.cc"
#if DEBUG
    let apiBaseUri = "http://10.5.86.139:8080"
#else
    let apiBaseUri = "https://jrs.y2k.work"
#endif
