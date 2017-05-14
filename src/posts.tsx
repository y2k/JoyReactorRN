import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView, ActivityIndicator,
    TouchableHighlight, Image, ListView, Dimensions, TouchableOpacity
} from 'react-native'

import { Post, Attachment, Domain, Loader as L, TagSource, FeedSource, PostsStates } from './domain'
import { PostDetailsComponent } from "./post"
import { NavigationComponent, TitleComponent } from "./components"

interface State { state: PostsStates }
interface ItemPost { kind: "post", value: Post }
interface ItemNext { kind: "next" }
type Item = ItemPost | ItemNext

export class PostsComponent extends Component<any, State> {

    async componentDidMount() {
        await L.reset() // TODO:

        const state = await L.loadFromStore({ kind: "feed" })
        this.setState({ state: state })
        const webState = await L.loadNext(state, { kind: "feed" })
        this.setState({ state: webState })
    }

    render() {
        return (
            <View style={{ flex: 1 }}>
                <TitleComponent title="Лента" />
                {this.state == null && this.loadingComponent()}
                {this.state != null && this.postListComponent()}
            </View>
        )
    }

    loadingComponent = () =>
        <ActivityIndicator style={{ flex: 1 }} size="large" color="#ffb100" />

    postListComponent() {
        return (
            <ListView
                enableEmptySections={true}
                dataSource={new ListView.DataSource({ rowHasChanged: (r1, r2) => r1 !== r2 })
                    .cloneWithRows(this.toUiState())}
                renderRow={(rowData: Item) => {
                    switch (rowData.kind) {
                        case "next": return (<NextPageComponent onClick={() => this.loadNextPage()} />)
                        case "post": return (<PostComponent data={rowData} />)
                    }
                }}
            />
        )
    }

    toUiState() {
        const state = this.state.state
        return state.posts
            .map<Item>(x => ({ kind: "post", value: x }))
            .concat([{ kind: "next" }])
            .concat(state.old.map<Item>(x => ({ kind: "post", value: x })))
    }

    async loadNextPage() {
        this.setState({ state: await L.loadNext(this.state.state, { kind: "feed" }) })
    }
}

interface ButtonProps { onClick: () => void }
class NextPageComponent extends Component<ButtonProps, any> {
    render() {
        return (
            <TouchableOpacity
                style={{
                    margin: 4,
                    backgroundColor: "#e49421",
                    borderRadius: 4,
                    overflow: "hidden",
                }}
                onPress={this.props.onClick}>
                <Text style={{
                    fontWeight: "bold",
                    fontSize: 13,
                    textAlign: "center",
                    padding: 15,
                    color: "white"
                }}>Load next page</Text>
            </TouchableOpacity>
        )
    }
}

interface PostsProps { data: ItemPost }

class PostComponent extends Component<PostsProps, any> {
    render(): any {
        const post = this.props.data.value
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