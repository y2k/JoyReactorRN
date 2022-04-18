FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

WORKDIR /app
COPY . /app

RUN dotnet test
RUN cd server && dotnet publish -c Release -r linux-x64 --self-contained false

RUN apk add yarn
RUN cd web && yarn && yarn webpack --mode production

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

EXPOSE 8090

WORKDIR /app
COPY --from=0 /app/server/bin/Release/net6.0/linux-x64/publish .
COPY --from=0 /app/web/public ./public

ENTRYPOINT ["dotnet", "server.dll"]
