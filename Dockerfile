FROM y2khub/fable-ci

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
    -H "X-HockeyAppToken: $HOCKEYAPP_TOKEN" \
    https://rink.hockeyapp.net/api/2/apps/upload

