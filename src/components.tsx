import React, { Component } from 'react'
import { View, Platform, TouchableOpacity, Text, TouchableHighlight, StyleSheet } from 'react-native'

interface TitleProps { title: string }

export class TitleComponent extends Component<TitleProps, any> {
    render() {
        return (
            <View>
                <View style={{ backgroundColor: "#ffb100", height: Platform.select({ ios: 15 }) }} />
                <View style={{ backgroundColor: "#ffb100", height: 50, flexDirection: "row", alignItems: "center" }}>
                    <TouchableOpacity
                        style={{ alignItems: "center", flexDirection: "row", width: 50, height: 50 }}
                        onPress={() => { }}>
                        <Text style={{
                            fontFamily: "icomoon",
                            flex: 1, textAlign: "center", color: "white", fontSize: 20
                        }}>{"\uea40"}</Text>
                    </TouchableOpacity>
                    <Text style={{ color: "white", fontSize: 18 }}>{this.props.title}</Text>
                </View>
            </View>
        );
    }
}

export class NavigationComponent extends Component<any, any> {
    render() {
        return (
            <View style={{ backgroundColor: "#ffb100", flexDirection: "row", height: 50 }}>
                <TouchableHighlight underlayColor="#f82" style={styles.tab} onPress={() => { }}>
                    <Text style={styles.tabText}>Лента</Text>
                </TouchableHighlight>
                <TouchableHighlight underlayColor="#f82" style={styles.tab} onPress={() => { }}>
                    <Text style={styles.tabText}>Теги</Text>
                </TouchableHighlight>
                <TouchableHighlight underlayColor="#f82" style={styles.tab} onPress={() => { }}>
                    <Text numberOfLines={1} style={styles.tabText}>Сообщения</Text>
                </TouchableHighlight>
                <TouchableHighlight underlayColor="#f82" style={styles.tab} onPress={() => { }}>
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