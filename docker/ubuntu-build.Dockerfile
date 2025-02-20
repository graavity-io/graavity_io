FROM graavity-base:ubuntu-16.04

COPY ./stack-docker.yaml /graavity/stack.yaml
COPY ./submodules/ /graavity/submodules/
COPY ./graavity.cabal /graavity/graavity.cabal

RUN cd /graavity && stack build --only-snapshot && stack build --only-dependencies

COPY ./Setup.hs /graavity/Setup.hs
COPY ./conf /graavity/conf
COPY ./demo /graavity/demo
COPY ./executables /graavity/executables
COPY ./graavityclient.sh /graavity/graavityclient.sh
COPY ./demo /graavity/demo
COPY ./src /graavity/src
COPY ./tests /graavity/tests
COPY ./LICENSE /graavity/LICENSE

ARG flag

RUN bash -c "mkdir -p /graavity/log && \
    cd && source ./build-exports && \
    cd /graavity && \
    stack install $flag"

RUN mkdir -p /ubuntu-16.04 && \
    cp /graavity/bin/genconfs /ubuntu-16.04 && \
    cp /graavity/bin/graavityserver /ubuntu-16.04 && \
    cp /graavity/bin/graavityclient /ubuntu-16.04

CMD ["/bin/bash"]
