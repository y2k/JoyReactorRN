import { PostsComponent } from "./posts"
import { StackNavigator } from "react-navigation"

export const App = StackNavigator({
  Home: { screen: PostsComponent },
})