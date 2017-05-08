import { AsyncStorage, Dimensions } from 'react-native'

export interface Attachment { url: string, aspect: number }
export interface Comment { text: string }
export interface Post { title: string, image: Attachment, comments: Comment[] }
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

export namespace Loader {

    export function loadProfile(name: string): Promise<Profile> {
        return request(`/user/${name}`, "profile")
    }

    export function postDescription(id: number): Promise<Post> {
        return request(Domain.urlPostDetails(id), "post")
    }

    export function posts(tag: Source): Promise<PostResponse> {
        return request(Domain.makeUrl(tag), "posts")
    }

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

    export function urlPostDetails(post: number) {
        return `/post/${post}`
    }

    export function makeUrl(tag: Source) {
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