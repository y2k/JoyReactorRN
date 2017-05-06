import React, { Component } from 'react'
import {
    StyleSheet, Text, View, Button, ScrollView,
    TouchableHighlight, Image, ListView, Dimensions, Platform
} from 'react-native'

interface TitleProps { title: string }

export class TitleComponent extends Component<TitleProps, any> {
    render() {
        return (
            <View>
                <View style={{ height: Platform.select({ ios: 15 }) }} />
                <View style={{ height: 50, flexDirection: "row", alignItems: "center" }}>
                    <TouchableHighlight style={{ alignItems: "center", flexDirection: "row", width: 50, height: 50 }} onPress={() => { }}>
                        <Text style={{ flex: 1, textAlign: "center", color: "white", fontSize: 30 }}>{"<"}</Text>
                    </TouchableHighlight>
                    <Text style={{ color: "white", fontSize: 18 }}>{this.props.title}</Text>
                </View>
            </View>
        );
    }
}

export class NavigationComponent extends Component<any, any> {
    render() {
        return (
            <View style={{ flexDirection: "row", height: 50 }}>
                <TouchableHighlight style={styles.tab} onPress={() => { }}>
                    <Text style={styles.tabText}>Лента</Text>
                </TouchableHighlight>
                <TouchableHighlight style={styles.tab} onPress={() => { }}>
                    <Text style={styles.tabText}>Теги</Text>
                </TouchableHighlight>
                <TouchableHighlight style={styles.tab} onPress={() => { }}>
                    <Text numberOfLines={1} style={styles.tabText}>Сообщения</Text>
                </TouchableHighlight>
                <TouchableHighlight style={styles.tab} onPress={() => { }}>
                    <Text style={styles.tabText}>Профиль</Text>
                </TouchableHighlight>
            </View>
        );
    }
}

const styles = StyleSheet.create({
    tab: {
        flex: 1,
        flexDirection: "row",
        alignItems: "center",
    },
    tabText: {
        fontSize: 14,
        color: "white",
        textAlign: "center",
        flex: 1,
    },
});