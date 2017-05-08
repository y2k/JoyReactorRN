import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions, ActivityIndicator
} from 'react-native'
import { Post, Attachment, Domain, Loader as L, TagSource, FeedSource, Comment } from './domain'
import { TitleComponent } from "./components"

interface PostState { kind: "post", post: Post }
interface ErrorState { kind: "error", message: string }
type State = PostState | ErrorState

export class PostDetailsComponent extends Component<any, State> {

    componentDidMount() {
        L.postDescription(3092583)
            .then(x => this.setState({ kind: "post", post: x }))
            .catch(x => this.setState({ kind: "error", message: JSON.stringify(x) }))
    }

    render() {
        return (
            <ScrollView style={{ backgroundColor: '#ffb100' }}>
                <TitleComponent title="Post" />
                {this.getViewState()}
            </ScrollView>
        )
    }

    getViewState() {
        if (this.state == null) return (
            <ActivityIndicator
                style={{ alignSelf: "center" }}
                size="large"
                color="white" />
        )

        switch (this.state.kind) {
            case "post": return (
                <View>
                    <Image
                        style={{ height: 200 }}
                        source={{ uri: this.state.post.image.url }} />
                    <Text>Лучшие комментарии:</Text>
                    {this.state.post.comments.map((x, i) => componentComment(x, i))}
                </View>
            );
            case "error": return (<Text>ERROR: {this.state.message}</Text>)
        }
    }
}

function componentComment(x: Comment, i: number) {
    return (<Text
        style={{ padding: 4, color: "white" }}
        numberOfLines={2} key={`comment${i}`}>{x.text}</Text>)
}