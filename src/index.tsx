import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'

import { Post, Attachment, Domain, Loader, TagSource, FeedSource } from './domain'
import { NavigationComponent, TitleComponent } from "./components"
import { PostDetailsComponent } from "./post"
import { PostsComponent } from "./posts"
import { ProfileComponent } from "./profile"

interface PostsProps { data: Post }

export default class App extends Component<any, any> {

    render() {
        return (
            <View style={styles.container}>
                <View style={{ flex: 1 }}>
                    {/*<PostDetailsComponent />*/}
                    {/*<PostsComponent />*/}
                    <ProfileComponent />
                </View>
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