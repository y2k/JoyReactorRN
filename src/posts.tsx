import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'

import { Post, Attachment, Domain, Loader, TagSource, FeedSource } from './domain'
import { PostDetailsComponent } from "./post"
import { NavigationComponent, TitleComponent } from "./components"

interface State { message: string, posts: Post[] }

export class PostsComponent extends Component<any, State> {

    state = { message: '...', posts: [] as Post[] }

    componentDidMount() {
        Loader.posts({ kind: "feed" })
            .then(x => { this.setState({ posts: x.posts }) })
            .catch(x => this.setState({ message: "ERROR: " + x }))
    }

    render() {
        return (
            <View style={{ flex: 1 }}>
                <TitleComponent title="Feed" />
                <ListView
                    enableEmptySections={true}
                    dataSource={
                        new ListView.DataSource({ rowHasChanged: (r1, r2) => r1 !== r2 })
                            .cloneWithRows(this.state.posts)}
                    renderRow={(rowData) => <PostComponent data={rowData} />} />
            </View>
        )
    }
}

interface PostsProps { data: Post }

class PostComponent extends Component<PostsProps, any> {
    render(): any {
        const post = this.props.data
        const image = Domain.normalizeUrl(post.image)
        const h = Domain.height(post.image)
        return (
            <TouchableHighlight
                style={{ paddingBottom: 0, paddingTop: 0 }}
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