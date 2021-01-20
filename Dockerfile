# See .github/workflows/tests.yml

ARG NODE_TAG
ARG PURESCRIPT_VER
ARG SPAGO_VER

FROM node:${NODE_TAG}

RUN apt-get update && apt-get install --no-install-recommends -y libtinfo5 netbase ca-certificates git

RUN npm i --unsafe-perm -g purescript@${PURESCRIPT_VER} spago@${SPAGO_VER}
COPY spago.dhall packages.dhall /app/

WORKDIR /app
RUN spago build --no-color
COPY . /app

CMD ["spago", "test", "--no-color"]
