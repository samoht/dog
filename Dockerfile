FROM ocaml/opam:alpine

MAINTAINER Thomas Gazagnaire <thomas@gazagnaire.org>

RUN sudo apk update

COPY . /home/opam/src/dog

RUN sudo chown -R opam.nogroup /home/opam/src && \
     opam pin add dog /home/opam/src/dog -n && \
     opam depext dog && \
     opam install dog
RUN sudo ln -s /home/opam/.opam/system/bin/dog /usr/bin/dog

ENTRYPOINT ["/usr/bin/dog"]
