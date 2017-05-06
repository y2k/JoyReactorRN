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
                <View style={{ padding: 4, flexDirection: "column" }} >
                    <TextInput style={style.edit}
                        placeholder="Логин"
                        underlineColorAndroid="transparent"
                        placeholderTextColor="white" />
                    <TextInput style={style.edit}
                        placeholder="Пароль"
                        underlineColorAndroid="transparent"
                        placeholderTextColor="white" />
                    <Button title="Войти" color="#b80" onPress={() => { }} />
                </View>
            </View>
        )
    }
}

const style = StyleSheet.create({
    edit: {
        padding: 4,
        height: 45,
        fontSize: 18,
        borderColor: "gray",
        borderWidth: 1,
        borderRadius: 10
    }
})