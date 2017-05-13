import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions, TouchableOpacity
} from 'react-native'

import { Post, Attachment, Domain, Loader as L, TagSource, FeedSource } from './domain'
import { PostDetailsComponent } from "./post"
import { NavigationComponent, TitleComponent } from "./components"

interface State { posts: Post[] }

export class PostsComponent extends Component<any, State> {

    state = { posts: [] as Post[] }

    componentDidMount() {
        L.syncPosts({ kind: "feed" })
            .catch(x => console.warn("ERROR: " + x))
        L.posts({ kind: "feed" })
            .then(x => this.setState({ posts: x.posts }))
    }

    render() {
        return (
            <View style={{ flex: 1 }}>
                <TitleComponent title="Лента" />
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
            <TouchableOpacity style={{ margin: 4 }} onPress={() => { }}>
                <View style={{
                    alignItems: "stretch",
                    backgroundColor: "white", borderColor: "#eee",
                    borderWidth: 1, borderRadius: 8, overflow: "hidden",
                }}>
                    {image != null &&
                        <Image
                            style={{ height: h, borderTopLeftRadius: 8, borderTopRightRadius: 8 }}
                            source={{ uri: image }} />
                    }
                    {/*{post.title != null &&
                        <Text numberOfLines={2} style={styles.text}>
                            {post.title}
                        </Text>
                    }*/}
                    <View style={{ flexDirection: "row", margin: 9 }}>
                        <Image
                            style={{ width: 36, height: 36, borderRadius: 18, marginRight: 9 }}
                            source={{ uri: post.userImage.url }} />
                        <View style={{ flex: 1 }}>
                            <Text style={{ fontWeight: "bold", fontSize: 14, color: "#616161" }}>{post.userName}</Text>
                            <View style={{ alignSelf: "flex-end", flexDirection: "row" }}>
                                <Text style={{ fontFamily: "icomoon", color: "#ffb100" }}>{"\ue8b5"}</Text>
                                <Text style={{ marginLeft: 8, color: "#bcbcbc" }}>2 часа</Text>
                            </View>
                        </View>
                    </View>
                </View>
            </TouchableOpacity >
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