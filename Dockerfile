ARG NODE_TAG=14-buster-slim
ARG PURESCRIPT_VER=0.13.8
ARG SPAGO_VER=0.15.3

FROM node:${NODE_TAG}

RUN apt-get update && apt-get install --no-install-recommends -y libtinfo5 netbase ca-certificates

RUN npm i --unsafe-perm -g purescript@${PURESCRIPT_VER} spago@${SPAGO_VER}
COPY spago.dhall packages.dhall /app/

WORKDIR /app
RUN spago build --no-color
COPY . /app

CMD ["spago", "test", "--no-color"]
