import React, { Component } from 'react'
import { ScrollView, ActivityIndicator, View, Image, Text } from 'react-native'
import { Post, Attachment, Domain, Loader as L, TagSource, FeedSource, Comment } from './domain'
import { TitleComponent } from "./components"

interface State { post: Post }

export class PostDetailsComponent extends Component<any, State> {

    componentDidMount() {
        L.postDescription(3092583)
            .then(x => this.setState({ post: x }))
            .catch(error => console.warn(error))
    }

    render() {
        return (
            <View style={{ flex: 1 }}>
                <TitleComponent title="Post" />
                {this.state == null && this.loadingComponent()}
                {this.state != null && this.getViewState()}
            </View>)
    }

    loadingComponent() {
        return (
            <ActivityIndicator
                style={{ flex: 1 }}
                size="large"
                color="#ffb100" />)
    }

    getViewState() {
        return (
            <ScrollView>
                <Image
                    style={{ height: 200 }}
                    source={{ uri: this.state.post.image.url }} />
                <Text>Лучшие комментарии:</Text>
                {this.state.post.comments.map((x, i) => componentComment(x, i))}
            </ScrollView>)
    }
}

function componentComment(x: Comment, i: number) {
    return (
        <Text
            style={{ padding: 4, color: "white" }}
            numberOfLines={2} key={`comment${i}`}>{x.text}
        </Text>)
}