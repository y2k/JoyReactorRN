import { AsyncStorage, Dimensions } from 'react-native'

export interface Attachment { url: string, aspect: number }
export interface Post { title: string, image: Attachment }
interface PostResponse { posts: Post[] }

export interface TagSource { kind: "tags", name: string }
export interface FeedSource { kind: "feed" }
export type Source = FeedSource | TagSource

export namespace Loader {

    export function posts(tag: Source): Promise<PostResponse> {
        return fetch(Domain.makeUrl(tag))
            .then(x => x.text())
            .then(x => {
                let form = new FormData()
                form.append("html", x)

                let request = {
                    method: "POST",
                    headers: { "Content-Type": "multipart/form-data" },
                    body: form
                }

                return fetch("http://212.47.229.214:4567/posts", request)
            })
            .then(x => x.json())
    }
}

export namespace Domain {

    export function makeUrl(tag: Source) {
        switch (tag.kind) {
            case "feed": return "http://joyreactor.cc/"
            case "tags": return `http://joyreactor.cc/tag/${encodeURIComponent(tag.name)}`
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