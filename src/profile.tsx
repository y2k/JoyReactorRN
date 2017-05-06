import React, { Component } from 'react'
import { View, StyleSheet, TextInput, Button } from 'react-native'

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
                <View style={{ paddingLeft: 30, paddingRight: 30, paddingTop: 25, flexDirection: "column" }} >
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
                    <Button title="Войти" color="#b80" onPress={() => { }} />
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
        fontSize: 18,
        paddingLeft: 18,
        borderColor: "#ececec",
        borderWidth: 1,
        borderRadius: 2
    }
})