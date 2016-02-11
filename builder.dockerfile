FROM fpco/stack-build:latest

RUN stack update

ADD ./hshort.cabal /app/hshort.cabal
WORKDIR /app

ADD . /app/
RUN stack build
RUN cp `stack paths --local-install-dir`/bin/hshort /app/hshort
