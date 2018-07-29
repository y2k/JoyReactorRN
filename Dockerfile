FROM microsoft/dotnet:2.1-sdk

# node, yarn
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs
RUN apt-get install -y build-essential
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt-get update && apt-get install yarn

# mono
RUN apt install apt-transport-https dirmngr
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb https://download.mono-project.com/repo/debian stable-stretch main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN apt update
RUN apt install -y mono-devel

# java
RUN apt-get update && apt-get install zip unzip && \
    curl -s "https://get.sdkman.io" | bash && \
    /bin/bash -c "source $HOME/.sdkman/bin/sdkman-init.sh; \
    sdk install java;"
ENV PATH=$PATH:/root/.sdkman/candidates/java/current/bin

# android
RUN apt-get install -y lib32stdc++6 lib32z1
ENV ANDROID_SDK_VERSION 4333796
RUN mkdir -p /opt/android-sdk && cd /opt/android-sdk && \
    wget -q https://dl.google.com/android/repository/sdk-tools-linux-${ANDROID_SDK_VERSION}.zip && \
    unzip *tools*linux*.zip && \
    rm *tools*linux*.zip
ENV ANDROID_HOME /opt/android-sdk
#RUN yes | $ANDROID_HOME/tools/bin/sdkmanager --licenses
RUN yes | $ANDROID_HOME/tools/bin/sdkmanager --licenses
#RUN $ANDROID_HOME/tools/bin/sdkmanager --update
RUN $ANDROID_HOME/tools/bin/sdkmanager  "platforms;android-27" "build-tools;23.0.1" "extras;google;m2repository" "extras;android;m2repository"
#RUN $ANDROID_HOME/tools/bin/sdkmanager --update
#RUN yes | $ANDROID_HOME/tools/bin/sdkmanager --licenses
#RUN $ANDROID_HOME/tools/bin/sdkmanager --update sdk

ARG APP_RELEASE_STORE_PASSWORD
ARG HOCKEYAPP_TOKEN

WORKDIR /app
COPY . /app

RUN yarn --ignore-optional
RUN dotnet restore -nowarn:NU1605,NU1103
RUN cd src && dotnet fable npm-build
RUN cd android && ./gradlew assembleRelease

RUN curl \
    -F "status=2" \
    -F "notify=1" \
    -F "ipa=@android/app/build/outputs/apk/app-release.apk" \
    -H 'X-HockeyAppToken: ${HOCKEYAPP_TOKEN}' \
    https://rink.hockeyapp.net/api/2/apps/upload

