FROM node:14-buster-slim

RUN apt-get update && apt-get install --no-install-recommends -y libtinfo5 netbase ca-certificates

RUN npm i --unsafe-perm -g purescript@0.13.8 spago@0.15.3
COPY spago.dhall packages.dhall /app/

WORKDIR /app
RUN spago build
COPY . /app

CMD ["spago", "test"]
