FROM haskell:latest

COPY stack.yaml /species-backend/stack.yaml
COPY species-backend.cabal /species-backend/species-backend.cabal
WORKDIR /species-backend

RUN stack setup

COPY . /species-backend

RUN stack build

RUN stack install

ENV DATABASE_FILEPATH="/database/species-db.sqlite"
ENTRYPOINT ["stack", "run", "species-backend"]
