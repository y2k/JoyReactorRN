import { AsyncStorage as AS, Dimensions } from "react-native"
import { Post, Source, Posts_, Profile, Attachment } from "types"

// export interface Profile {
//     userName: string,
//     userImage: Attachment,
//     rating: number,
//     stars: number,
//     progressToNewStar: number
// }
// export interface Attachment { url: string, aspect: number }
// export interface Comment { text: string, image: Attachment, rating: number }
// export interface Post {
//     id: number,
//     userName: string,
//     userImage: Attachment,
//     rating: number,
//     created: number,
//     title: string,
//     image: Attachment,
//     comments: Comment[]
// }

interface PostResponse { posts: Post[], nextPage: number }

// export interface TagSource { kind: "tags", name: string }
// export interface FeedSource { kind: "feed" }
// export type Source = FeedSource | TagSource

// export interface PostsFromCache {
//     readonly kind: "PostsFromCache",
//     readonly source: Source,
//     readonly posts: Post[],
// }

// export interface PostsFromCachedAndWeb {
//     readonly kind: "PostsFromCachedAndWeb",
//     readonly source: Source,
//     readonly posts: Post[],
//     readonly preloadedPosts: Post[],
//     readonly next: number | null,
// }

// export interface PostsWithNextPage {
//     readonly kind: "PostsWithNextPage",
//     readonly source: Source,
//     readonly posts: Post[],
//     readonly oldPosts: Post[],
//     readonly next: number | null,
// }

// export interface PostsError { readonly kind: "PostsError" }

// export type Posts_ = PostsFromCache | PostsFromCachedAndWeb | PostsWithNextPage | PostsError

module PostsFunctions {

    export const clearNewPostsFromOld = (old: Post[], posts: Post[]): Post[] =>
        old.filter(x => posts.every(i => i.id != x.id))

    export const mergeNextPage = (posts: Post[], webPosts: Post[]): Post[] =>
        posts.concat(webPosts.filter(x => posts.every(i => i.id != x.id)))

    export const mergeNextPage_ = (old: Post[], posts: Post[], webPosts: Post[]): Post[] =>
        old.filter(x => posts.every(i => i.id != x.id) && webPosts.every(i => i.id != x.id))
}

export module Loader {


    export const preload = async (source: Source): Promise<Posts_> => {
        // JSON.parse(await AS.getItem("state"))
        return {
            kind: "PostsFromCache",
            source: source,
            posts: [],
        }
    }

    export const next = async (state: Posts_): Promise<Posts_> => {
        switch (state.kind) {
            case "PostsFromCache": {
                const web = await Loader.request<PostResponse>(Domain.postsUrl(state.source, null))
                return {
                    kind: "PostsFromCachedAndWeb",
                    source: state.source,
                    posts: state.posts,
                    preloadedPosts: web.posts,
                    next: web.nextPage,
                }
            }
            case "PostsFromCachedAndWeb":
                return {
                    kind: "PostsWithNextPage",
                    source: state.source,
                    posts: state.preloadedPosts,
                    oldPosts: PostsFunctions.clearNewPostsFromOld(state.posts, state.preloadedPosts),
                    next: state.next,
                }
            case "PostsWithNextPage": {
                const web = await Loader.request<PostResponse>(Domain.postsUrl(state.source, state.next))
                return {
                    kind: "PostsWithNextPage",
                    source: state.source,
                    posts: PostsFunctions.mergeNextPage(state.posts, web.posts),
                    oldPosts: PostsFunctions.mergeNextPage_(state.oldPosts, state.posts, web.posts),
                    next: web.nextPage
                }
            }
        }
        return state
    }

    export const debugReset = () => AS.clear()

    /**
     * 
     */

    export const loadProfile = (name: string): Promise<Profile> =>
        request(Domain.profileUrl(name))

    export const postDescription = (id: number): Promise<Post> =>
        request(Domain.postDetailsUrl(id))

    export function request<T>(api: ParserApi): Promise<T> {
        return fetch(`http://joyreactor.cc${api.path}`)
            .then(x => x.text())
            .then(x => {
                const form = new FormData()
                form.append("html", x)

                const request = {
                    method: "POST",
                    headers: { "Content-Type": "multipart/form-data" },
                    body: form
                }

                return fetch(`http://212.47.229.214:4567/${api.parser}`, request)
            })
            .then(x => x.json())
    }
}

interface ParserApi { readonly path: string, readonly parser: string }

export namespace Domain {

    export const profileUrl = (name: string): ParserApi =>
        ({ path: `/user/${encodeURIComponent(name)}`, parser: "profile" })
    export const postDetailsUrl = (post: number): ParserApi =>
        ({ path: `/post/${post}`, parser: "post" })

    export function postsUrl(tag: Source, page: number | null): ParserApi {
        if (page == null) // TODO: объединить логику
            switch (tag.kind) {
                case "feed": return { path: "/", parser: "posts" }
                case "tags": return { path: `/tag/${encodeURIComponent(tag.name)}`, parser: "posts" }
            }
        switch (tag.kind) {
            case "feed": return { path: `/${page}`, parser: "posts" }
            case "tags": return { path: `/tag/${encodeURIComponent(tag.name)}/${page}`, parser: "posts" }
        }
    }

    export function height(x: Attachment) {
        const w = Dimensions.get("screen").width
        return x ? w / Math.max(1.2, x.aspect) : 0
    }

    export function normalizeUrl(x: Attachment) {
        const prefix = "http://rc.y2k.work/cache/fit?width=300&height=200&bgColor=ffffff&quality=75&url="
        return x ? prefix + encodeURIComponent(x.url) : null
    }
}