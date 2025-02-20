FROM graavity-base:centos-6.8

COPY ./stack-docker.yaml /graavity/stack.yaml
COPY ./submodules/ /graavity/submodules/
COPY ./graavity.cabal /graavity/graavity.cabal

RUN source /home/build-exports && ldconfig && cd /graavity && stack build --only-snapshot

RUN source /home/build-exports && ldconfig && cd /graavity && stack build --only-dependencies

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
    cd && source /home/build-exports && ldconfig && \
    cd /graavity && \
    stack install $flag"


RUN mkdir -p /centos-6.8 && \
    cp graavity/bin/genconfs /centos-6.8 && \
    cp graavity/bin/graavityserver /centos-6.8 && \
    cp graavity/bin/graavityclient /centos-6.8

CMD ["/bin/bash"]
