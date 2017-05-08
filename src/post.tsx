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
                <TitleComponent title={this.state && this.state.post.title} />
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
                <Text style={{ padding: 13 }}>Лучшие комментарии:</Text>
                {this.state.post.comments.map((x, i) => componentComment(x, i))}
            </ScrollView>)
    }
}

function componentComment(x: Comment, i: number) {
    return (
        <View key={`comment-${i}`} style={{ paddingBottom: 15, flexDirection: "row", paddingLeft: 13, paddingRight: 13 }}>
            <Image
                style={{ marginRight: 13, width: 36, height: 36, borderRadius: 18 }}
                source={{ uri: x.image.url }} />
            <View style={{ flex: 1 }}>
                <Text style={{ color: "#999", includeFontPadding: false }} numberOfLines={3}>
                    {x.text}
                </Text>
                <View style={{ marginTop: 8, flexDirection: "row", alignSelf: "flex-end" }}>
                    <Text style={{ fontFamily: "icomoon", color: "#ffb100" }}>{"\ue8dc"}</Text>
                    <Text style={{ marginLeft: 8, color: "#616161" }}>{x.rating}</Text>
                </View>
            </View>
        </View>)
}