$ANDROID_HOME/platform-tools/adb reverse tcp:8081 tcp:8081

cd src
dotnet fable npm-cold-start
cd ..

react-native run-android

cd src
dotnet fable npm-start
