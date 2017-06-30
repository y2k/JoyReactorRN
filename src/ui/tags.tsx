import React, { Component } from 'react'
import { ListView, View, StyleSheet, Text, TouchableOpacity, Image } from 'react-native'
import { Loader as L } from '../domain/domain'
import { Tag } from '../domain/types'

export class TagsComponent extends Component<Tag[], any> {

    static navigationOptions = {
        title: "Теги",
        headerTintColor: "white",
        headerStyle: { backgroundColor: "#ffb100" }
    }

    async componentDidMount() {
        this.setState(await L.tags())
    }

    render() {
        return (
            <View style={{ flex: 1, backgroundColor: "#fafafa" }}>
                <ListView
                    enableEmptySections={true}
                    dataSource={new ListView.DataSource({ rowHasChanged: (r1, r2) => r1 !== r2 })
                        .cloneWithRows(this.state || [])}
                    renderRow={(rowData: Tag) =>
                        <TouchableOpacity activeOpacity={0.4} onPress={() => console.log("pressed")}>
                            <View style={{ flexDirection: "row", padding: 8 }}>
                                <Image style={style.image} source={{ uri: rowData.image }} />
                                <Text style={style.label}>{rowData.title}</Text>
                            </View>
                        </TouchableOpacity>
                    } />
            </View>
        )
    }
}

const style = StyleSheet.create({
    image: { width: 48, height: 48, borderRadius: 24, marginRight: 8 },
    label: { fontSize: 18, color: "#404040", alignSelf: "center" },
})