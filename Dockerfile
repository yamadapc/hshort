FROM haskell:7.10

RUN cabal update

ADD ./hshort.cabal /app/hshort.cabal
RUN cd /app && cabal install --only-dep -j

ADD . /app
RUN cd /app && cabal install

WORKDIR /app

CMD hshort
