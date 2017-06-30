import { StackNavigator, TabNavigator } from "react-navigation"
import { PostsComponent } from "./posts"
import { PostDetailsComponent } from "./post"
import { ProfileComponent } from "./profile"
import { TagsComponent } from "./tags"

const MainScreenNavigator = TabNavigator({
  Home: { screen: PostsComponent },
  Tags: { screen: TagsComponent },
  Messages: { screen: ProfileComponent },
  Profile: { screen: ProfileComponent },
}, {
    tabBarOptions: {
      showIcon: false,
      activeTintColor: "white",
      inactiveTintColor: "#e0e0e0",
      labelStyle: {
        fontSize: 14,
      },
      style: {
        backgroundColor: '#ffb100',
      },
    }
  });

export const App = StackNavigator({
  Home: { screen: MainScreenNavigator },
  Post: { screen: PostDetailsComponent }
})