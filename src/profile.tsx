import React, { Component } from 'react'
import { View, StyleSheet, TextInput, Button, TouchableHighlight, Text, TouchableOpacity } from 'react-native'

import { Loader } from './domain'
import { NavigationComponent, TitleComponent } from "./components"

export class ProfileComponent extends Component<any, any> {
    render() { return (<LoginComponent />) }
}

class LoginComponent extends Component<any, any> {
    render() {
        return (
            <View >
                <TitleComponent title="Авторизация" />
                <View style={{ padding: 20, flexDirection: "column" }} >
                    <TextInput style={style.edit}
                        placeholder="Логин"
                        underlineColorAndroid="transparent"
                        placeholderTextColor="gray" />
                    <View style={{ height: 12 }} />
                    <TextInput style={style.edit}
                        placeholder="Пароль"
                        secureTextEntry={true}
                        underlineColorAndroid="transparent"
                        placeholderTextColor="gray" />
                    <View style={{ height: 12 }} />
                    <TouchableOpacity
                        style={{
                            backgroundColor: "#e49421",
                            borderRadius: 4,
                            overflow: "hidden",
                        }}
                        onPress={() => { }}>
                        <Text style={{
                            fontWeight: "bold",
                            fontSize: 13,
                            textAlign: "center",
                            padding: 15,
                            color: "white"
                        }}>ВОЙТИ</Text>
                    </TouchableOpacity>

                </View>
            </View>
        )
    }
}

const style = StyleSheet.create({
    edit: {
        backgroundColor: "white",
        color: "black",
        padding: 4,
        height: 45,
        fontSize: 16,
        paddingLeft: 18,
        borderColor: "#ececec",
        borderWidth: 1,
        borderRadius: 4
    }
})