import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions
} from 'react-native'

import { Post, Attachment, Domain, Loader, TagSource, FeedSource } from './domain'
import { NavigationComponent, TitleComponent } from "./components"

export class ProfileComponent extends Component<any, any> {
    render() { return (<LoginComponent />) }
}

class LoginComponent extends Component<any, any> {
    render() {
        return (
            <View>

            </View>
        )
    }
}