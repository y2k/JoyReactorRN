import React, { Component } from 'react'
import { View, StyleSheet } from 'react-native'
import { NavigationComponent } from "./components"
import { PostsComponent } from "./posts"

export default class App extends Component<any, any> {

    render() {
        return (
            <View style={styles.container}>
                <View style={{ flex: 1 }}>
                    <PostsComponent />
                    {/*<PostDetailsComponent />*/}
                    {/*<ProfileComponent />*/}
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
        backgroundColor: '#fafafa',
    },
})