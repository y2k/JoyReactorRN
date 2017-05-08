import React, { Component } from 'react'
import { View, StyleSheet, TextInput, Button, Text, TouchableOpacity, Image, ActivityIndicator } from 'react-native'

import { Loader as L, Profile } from './domain'
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

    componentDidMount() {
        L.loadProfile("_y2k")
            .then(x => this.setState({ profile: x }))
            .catch(error => console.warn(error))
    }

    render() {
        return (
            <View style={{ flex: 1 }}>
                <TitleComponent title="Профиль" />
                {this.state == null && this.emptyComponent()}
                {this.state != null && this.loadedComponent()}
            </View>)
    }

    loadedComponent() {
        return (
            <View>
                <Image
                    source={{ uri: this.state.profile.userImage.url }}
                    style={{ marginTop: 20, alignSelf: "center", height: 90, width: 90, borderRadius: 45 }} />
                <Text
                    style={{
                        alignSelf: "center",
                        marginTop: 20,
                        color: "#616161",
                        fontSize: 20,
                    }}
                >{this.state.profile.userName}</Text>
                <Text
                    style={{
                        alignSelf: "center",
                        marginTop: 8,
                        color: "#616161",
                        fontSize: 13,
                    }}
                >Рейтинг: {this.state.profile.rating}</Text>

                <View style={{ height: 10 }} />

                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />
                <View style={{ backgroundColor: "white", height: 50, flexDirection: "row", alignItems: "center", justifyContent: "center" }} >
                    <Text style={{ includeFontPadding: false, fontSize: 25, color: "#edc95b" }}>
                        {"★".repeat(this.state.profile.stars)}
                    </Text>
                    <Text style={{ includeFontPadding: false, fontSize: 25, color: "#e4e6e7" }}>
                        {"★".repeat(Math.max(0, 10 - this.state.profile.stars))}
                    </Text>
                </View>
                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />
                <View style={{ backgroundColor: "white", padding: 20 }} >
                    <Text style={{ textAlign: "center", color: "#616161" }}>Прогресс до следующей звезды:</Text>
                    <View style={{ borderRadius: 4, marginTop: 12, height: 21, backgroundColor: "#e4e4e4" }}>
                        <View style={{ width: `${this.state.profile.progressToNewStar}%`, borderRadius: 4, height: 21, backgroundColor: "#edc95b" }} />
                    </View>
                </View>
                <View style={{ backgroundColor: "#e4e4e4", height: 1 }} />

                <View style={{ height: 10 }} />
                <ButtonComponent title="Выйти" margin={20} />
            </View>
        )
    }

    emptyComponent() {
        return (
            <ActivityIndicator
                style={{ flex: 1 }}
                size="large"
                color="#ffb100" />
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