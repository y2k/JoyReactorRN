import React, { Component } from 'react'
import { View, StyleSheet, TextInput, Button, Text, TouchableOpacity, Image } from 'react-native'

import { Loader, Profile } from './domain'
import { NavigationComponent, TitleComponent } from "./components"

export class ProfileComponent extends Component<any, any> {
    render() {
        return (
            // <LoginComponent />
            <UserUnfoComponent />
        )
    }
}

interface ProfileState { profile: Profile }

class UserUnfoComponent extends Component<any, ProfileState> {

    state = { profile: { name: "Alex", avatar: "http://img1.joyreactor.cc/pics/avatar/tag/10415" } }

    render() {
        return (
            <View>
                <TitleComponent title="Профиль" />
                <Image
                    source={{ uri: this.state.profile.avatar }}
                    style={{ marginTop: 20, alignSelf: "center", height: 90, width: 90, borderRadius: 45 }} />
                <Text
                    style={{
                        alignSelf: "center",
                        marginTop: 20,
                        color: "#616161",
                        fontSize: 20,
                    }}
                >{this.state.profile.name}</Text>
                <Text
                    style={{
                        alignSelf: "center",
                        marginTop: 8,
                        color: "#616161",
                        fontSize: 13,
                    }}
                >Рейтинг: 999</Text>

                <View style={{ height: 10 }} />

                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />
                <View style={{ backgroundColor: "white", height: 50, flexDirection: "row", alignItems: "center", justifyContent: "center" }} >
                    <Text style={{ fontSize: 25, color: "#edc95b" }}>★★★★★</Text>
                    <Text style={{ fontSize: 25, color: "#e4e6e7" }}>★★★★★</Text>
                </View>
                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />
                <View style={{ backgroundColor: "white", height: 50 }} />
                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />

                <View style={{ height: 10 }} />
                <ButtonComponent title="Выйти" margin={20} />
            </View>
        )
    }
}

class LoginComponent extends Component<void, void> {
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

                    <ButtonComponent title="Выйти" margin={0} />

                    {/*<TouchableOpacity
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
                    </TouchableOpacity>*/}
                </View>
            </View>
        )
    }
}

interface ButtonProps { title: string, margin: number }
class ButtonComponent extends Component<ButtonProps, any> {
    render() {
        return (
            <TouchableOpacity
                style={{
                    marginLeft: this.props.margin,
                    marginRight: this.props.margin,
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
                }}>{this.props.title.toUpperCase()}</Text>
            </TouchableOpacity>
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