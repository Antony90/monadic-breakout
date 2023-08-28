FROM haskell:latest

ENV DEBIAN_FRONTEND noninteractive
WORKDIR /game/

RUN apt-get update
# OpenGL for Gloss
RUN apt-get install -y freeglut3-dev
RUN cabal update

COPY app/* app/
COPY assets/* assets/
COPY assets/levels/* assets/levels/
COPY src/* src/
COPY test/* test/

COPY monadic-breakout.cabal .

RUN cabal install
RUN cabal build monadic-breakout-exe

ENTRYPOINT [ "cabal", "run", "monadic-breakout-exe" ]