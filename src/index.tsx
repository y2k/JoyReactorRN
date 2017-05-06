import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'

import { Post, Attachment, Domain, Loader, TagSource, FeedSource } from './domain'
import { NavigationComponent, TitleComponent } from "./components"
import { PostDetailsComponent } from "./post"
import { PostsComponent } from "./posts"

interface PostsProps { data: Post }

export default class App extends Component<any, any> {

    render() {
        return (
            <View style={styles.container}>
                <PostDetailsComponent />
                {/*<PostsComponent />*/}
                <NavigationComponent />
            </View>
        )
    }
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
        justifyContent: "center",
        backgroundColor: '#ffb100',
    },
});