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
    | { readonly kind: "fromCache", readonly source: Source, readonly posts: Post[], }
    | { readonly kind: "fromCachedAndWeb", readonly state: PostState, }
    | { readonly kind: "withNextPage", readonly state: PostState, }
    | { readonly kind: "error" }

export interface PostState {
    readonly source: Source,
    readonly posts: Post[],
    readonly bufferdPosts: Post[],
    readonly next: number | null,
}