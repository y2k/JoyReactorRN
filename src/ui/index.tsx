import { StackNavigator } from "react-navigation"
import { PostsComponent } from "./posts"
import { PostDetailsComponent } from "./post"

export const App = StackNavigator({
  Home: { screen: PostsComponent },
  Post: { screen: PostDetailsComponent }
})