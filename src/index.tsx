import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'

import { Post, Attachment, Domain, Loader, TagSource, FeedSource } from './domain'
import { PostDetailsComponent } from "./post"
import { NavigationComponent } from "./components"

interface PostsProps { data: Post }
interface State { message: string, posts: Post[] }

export default class App extends Component<any, State> {

    state = { message: '...', posts: [] as Post[] }

    componentDidMount() {
        Loader.posts({ kind: "feed" })
            .then(x => { this.setState({ posts: x.posts }) })
            .catch(x => this.setState({ message: "ERROR: " + x }))
    }

    render() {
        return (
            <View style={styles.container}>
                <PostDetailsComponent />
                <NavigationComponent />
            </View>
        )
    }
}

class PostComponent extends Component<PostsProps, any> {
    render(): any {
        const post = this.props.data
        const image = Domain.normalizeUrl(post.image)
        const h = Domain.height(post.image)
        return (
            <TouchableHighlight
                style={{ paddingBottom: 4, paddingTop: 4 }}
                underlayColor="#ffb100" onPress={() => { }}>
                <View style={{ alignItems: "stretch" }}>
                    {image != null &&
                        <Image
                            style={{ height: h }}
                            source={{ uri: image }} />
                    }
                    {post.title != null &&
                        <Text
                            numberOfLines={2}
                            style={styles.text}>{post.title}</Text>
                    }
                </View>
            </TouchableHighlight>
        );
    }
}

const styles = StyleSheet.create({
    tab: { flex: 1 },
    tabText: {
        fontSize: 18,
        color: "white",
        flex: 1,
        textAlign: "center",
        textAlignVertical: "center",
    },
    container: {
        flex: 1,
        justifyContent: "center",
        backgroundColor: '#ffb100',
    },
    text: {
        padding: 4,
        backgroundColor: '#df9100',
        color: "white",
        fontSize: 20,
        textAlign: 'center'
    },
});