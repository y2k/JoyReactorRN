import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'
import { Post, Attachment, Domain, Loader, TagSource, FeedSource, Comment } from './domain'

export class PostDetailsComponent extends Component<any, Post> {

    componentDidMount() {
        Loader.postDescription(3092583).then(x => this.setState(x))
    }

    render() {
        if (this.state == null) return (<Text>Loading...</Text>)
        const post = this.state
        return (
            <ScrollView style={styles.container}>
                <Text>post.title</Text>

                <Image
                    style={{ height: 200 }}
                    source={{ uri: post.image.url }} />

                {post.comments.map((x, i) => componentComment(x, i))}
            </ScrollView>
        );
    }
}

function componentComment(x: Comment, i: number) {
    return (<Text
        style={{ padding: 4, color: "white" }}
        numberOfLines={2} key={`comment${i}`}>{x.text}</Text>)
}

const styles = StyleSheet.create({
    container: {
        // flex: 1,
        // justifyContent: "center",
        backgroundColor: '#ffb100',
    }
})