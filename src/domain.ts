import { AsyncStorage, Dimensions } from 'react-native'

export interface Attachment { url: string, aspect: number }
export interface Comment { text: string, image: Attachment, rating: number }
export interface Post { userName: string, userImage: Attachment, rating: number, created: number, title: string, image: Attachment, comments: Comment[] }
interface PostResponse { posts: Post[] }

export interface TagSource { kind: "tags", name: string }
export interface FeedSource { kind: "feed" }
export type Source = FeedSource | TagSource

export interface Profile {
    userName: string,
    userImage: Attachment,
    rating: number,
    stars: number,
    progressToNewStar: number,
}

export module Loader {

    export const loadProfile = (name: string): Promise<Profile> =>
        request(Domain.profileUrl(name), "profile")

    export const postDescription = (id: number): Promise<Post> =>
        request(Domain.postDetailsUrl(id), "post")

    export const posts = (tag: Source): Promise<PostResponse> =>
        request(Domain.postsUrl(tag), "posts")

    function request<T>(path: string, parse: string): Promise<T> {
        return fetch(`http://joyreactor.cc${path}`)
            .then(x => x.text())
            .then(x => {
                const form = new FormData()
                form.append("html", x)

                const request = {
                    method: "POST",
                    headers: { "Content-Type": "multipart/form-data" },
                    body: form
                }

                return fetch(`http://212.47.229.214:4567/${parse}`, request)
            })
            .then(x => x.json())
    }
}

export namespace Domain {

    export const profileUrl = (name: string) => `/user/${encodeURIComponent(name)}`
    export const postDetailsUrl = (post: number) => `/post/${post}`

    export function postsUrl(tag: Source) {
        switch (tag.kind) {
            case "feed": return "/"
            case "tags": return `/tag/${encodeURIComponent(tag.name)}`
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