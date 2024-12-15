FROM haskell:9.6.6-bullseye AS builder
WORKDIR /usr/src/app
COPY ascii-tty-hs.cabal package.yaml stack.yaml stack.yaml.lock ./
RUN stack install --only-dependencies
COPY . .
RUN stack install

FROM scratch
COPY --from=builder /root/.local/bin/ascii-tty-hs-exe /ascii-tty-hs
ENTRYPOINT ["/ascii-tty-hs"]
