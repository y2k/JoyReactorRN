namespace JoyReactor

module Types =
    type Url = string

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
          attachments : AttachmentResource []
          id : int
          parentId : int }

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

    type PostResponse =
        { posts : Post []
          nextPage : int [] }
        static member empty : PostResponse = { posts = [||]; nextPage = [||] }

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

    type ParseResponse = 
        { profile : Profile option
          posts : PostResponse option
          userName : string option
          userTags : Tag [] option
          topTags : Tag [] option
          post : Post option
          messages : MessagesWithNext option }

    type PostForm =
        { url : string
          form : string }

module CofxStorage =
    open Types

    type LocalDb =
        { feeds : Map<Source, PostsWithLevels>
          sharedFeeds : PostResponse option
          posts : Map<int, Post>
          userName : string option
          userTags : Map<string, Tag>
          topTags : Map<string, Tag>
          messages : Map<double, Message>
          nextMessagesPage : string option
          profile : Profile option }
    with
        static member empty = { feeds = Map.empty; sharedFeeds = None; posts = Map.empty; userName = None; userTags = Map.empty; topTags = Map.empty; messages = Map.empty; nextMessagesPage = None; profile = None }
