FROM ocaml/opam:alpine

MAINTAINER Thomas Gazagnaire <thomas@gazagnaire.org>

RUN sudo apk update

COPY lib /home/opam/src/dog/lib
COPY bin /home/opam/src/dog/bin
COPY Makefile /home/opam/src/dog/Makefile
COPY _oasis /home/opam/src/dog/_oasis
COPY opam /home/opam/src/dog/opam

RUN sudo chown -R opam.nogroup /home/opam/src && \
     opam pin add dog /home/opam/src/dog -n && \
     opam depext dog && \
     opam install dog
RUN sudo ln -s /home/opam/.opam/system/bin/dog /usr/bin/dog

ENTRYPOINT ["/usr/bin/dog"]
