FROM mcr.microsoft.com/dotnet/core/sdk:3.1.102-alpine3.11

WORKDIR /app
COPY . /app

RUN dotnet test
RUN cd web-cli && dotnet publish -c Release -r linux-x64 --self-contained false

RUN apk add yarn
RUN cd web && yarn --ignore-optional && yarn webpack --mode production

FROM mcr.microsoft.com/dotnet/core/runtime:3.1.2-alpine3.11

WORKDIR /app
COPY --from=0 /app/web-cli/bin/Release/netcoreapp3.1/linux-x64/publish .
COPY --from=0 /app/web/public ./public

EXPOSE 8090

ENTRYPOINT ["dotnet", "web-cli.dll"]
