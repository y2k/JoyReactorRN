import React, { Component } from 'react'
import { Text, View, ActivityIndicator, Image, ListView, TouchableOpacity } from 'react-native'
import { Loader as L, Domain } from '../domain/domain'
import { Post, Posts } from '../domain/types'

interface ItemPost { kind: "post", value: Post }
interface ItemNext { kind: "next" }
type Item = ItemPost | ItemNext

export class PostsComponent extends Component<any, Posts> {

    static navigationOptions = {
        title: "Лента",
        headerTintColor: "white",
        headerStyle: {
            backgroundColor: "#ffb100"
        }
    }

    async componentDidMount() {
        this.setState(await L.preload({ kind: "feed" }))
        this.setState(await L.next(this.state))
    }

    render() {
        return (
            <View style={{ flex: 1, backgroundColor: "#fafafa" }}>
                {this.state == null && this.loadingComponent()}
                {this.state != null && this.postListComponent()}
            </View>)
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
                        case "post": return (<PostComponent
                            data={rowData}
                            onClick={() => this.props.navigation.navigate('Post', { id: rowData.value.id })} />)
                    }
                }} />)
    }

    toUiState(): Item[] {
        const state = this.state
        switch (state.kind) {
            case "cache":
                return state.posts.map<Item>(x => ({ kind: "post", value: x }))
            case "cachedAndWeb":
                return new Array<Item>()
                    .concat({ kind: "next" })
                    .concat(state.state.posts.map<Item>(x => ({ kind: "post", value: x })))
            case "nextPage":
                return state.state.posts.map<Item>(x => ({ kind: "post", value: x }))
                    .concat({ kind: "next" })
                    .concat(state.state.bufferdPosts.map<Item>(x => ({ kind: "post", value: x })))
        }
        return []
    }

    async loadNextPage() {
        this.setState(await L.next(this.state))
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
            </TouchableOpacity>)
    }
}

interface PostsProps { data: ItemPost, onClick: () => void }
class PostComponent extends Component<PostsProps, any> {
    render(): any {
        const post = this.props.data.value
        const image = Domain.normalizeUrl(post.image)
        const h = Domain.height(post.image)
        return (
            <TouchableOpacity activeOpacity={0.7} style={{ margin: 4 }} onPress={() => this.props.onClick()}>
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
            </TouchableOpacity >);
    }
}