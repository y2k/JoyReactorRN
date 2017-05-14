import { AsyncStorage as AS, Dimensions } from 'react-native'

export interface Profile { userName: string, userImage: Attachment, rating: number, stars: number, progressToNewStar: number }
export interface Attachment { url: string, aspect: number }
export interface Comment { text: string, image: Attachment, rating: number }
export interface Post { id: number, userName: string, userImage: Attachment, rating: number, created: number, title: string, image: Attachment, comments: Comment[] }
interface PostResponse { posts: Post[], nextPage: number }

export interface TagSource { kind: "tags", name: string }
export interface FeedSource { kind: "feed" }
export type Source = FeedSource | TagSource

export interface PostsStates {
    preloaded: Post[],
    posts: Post[],
    old: Post[],
    next: number | null,
}

module PostsFunctions {

    export function mergeToClear(state: PostsStates | null): PostsStates {
        if (state == null) return { preloaded: [], posts: [], old: [], next: null }
        return { preloaded: [], posts: [], old: state.posts.concat(state.old), next: null }
    }

    export function merge(state: PostsStates, posts: Post[], next: number): PostsStates {
        const actual = state.posts
            .map(x => posts.find(i => i.id == x.id) || x)
            .concat(posts.filter(x => !state.posts.some(i => i.id == x.id)))
        const old = state.old.filter(x => !posts.some(i => i.id == x.id))
        return { ...state, posts: actual, old: old, next: next }
    }
}

export module Loader {

    export const debugReset = () => AS.clear()

    export async function loadFromStorage(source: Source): Promise<PostsStates> {
        return PostsFunctions.mergeToClear(JSON.parse(await AS.getItem("state")))
    }

    export async function preload(source: Source): Promise<PostsStates> {
        const state = await loadFromStorage(source)
        const resp = await request<PostResponse>(Domain.postsUrl(source, state.next))
        const newState = PostsFunctions.merge(state, resp.posts, resp.nextPage)
        await AS.setItem("state", JSON.stringify(newState))
        return newState
    }

    export async function loadNext(state: PostsStates, source: Source): Promise<PostsStates> {
        const resp = await request<PostResponse>(Domain.postsUrl(source, state.next))
        const newState = PostsFunctions.merge(state, resp.posts, resp.nextPage)
        await AS.setItem("state", JSON.stringify(newState))
        return newState
    }

    /**
     * 
     */

    export const loadProfile = (name: string): Promise<Profile> =>
        request(Domain.profileUrl(name))

    export const postDescription = (id: number): Promise<Post> =>
        request(Domain.postDetailsUrl(id))

    function request<T>(api: ParserApi): Promise<T> {
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