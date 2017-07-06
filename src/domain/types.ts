export interface Profile {
    userName: string,
    userImage: Attachment,
    rating: number,
    stars: number,
    progressToNewStar: number
}

export interface Attachment { url: string, aspect: number }
export interface Comment { text: string, image: Attachment, rating: number }
export interface Post {
    id: number,
    userName: string,
    userImage: Attachment,
    rating: number,
    created: number,
    title: string,
    image: Attachment,
    comments: Comment[],
}

export interface Tag {
    name: string,
    image: string,
}

export type Source = FeedSource | TagSource
export interface TagSource { kind: "tags", name: string }
export interface FeedSource { kind: "feed" }

export type Posts = 
    | PostsFromCache
    | PostsFromCachedAndWeb
    | PostsWithNextPage
    | PostsError

export interface PostsFromCache {
    readonly kind: "PostsFromCache",
    readonly source: Source,
    readonly posts: Post[],
}

export interface PostsFromCachedAndWeb {
    readonly kind: "PostsFromCachedAndWeb",
    readonly state: PostState,
}

export interface PostsWithNextPage {
    readonly kind: "PostsWithNextPage",
    readonly state: PostState,
}

export interface PostState {
    readonly source: Source,
    readonly posts: Post[],
    readonly bufferdPosts: Post[],
    readonly next: number | null,
}

export interface PostsError { readonly kind: "PostsError" }