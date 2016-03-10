FROM samoht/dog

MAINTAINER Thomas Gazagnaire <thomas@gazagnaire.org>

# config sshd from https://github.com/macropin/docker-sshd

USER root
RUN apk update && \
    apk add bash git openssh && \
    mkdir -p ~root/.ssh && chmod 700 ~root/.ssh/ && \
    echo -e "Port 22\n" >> /etc/ssh/sshd_config && \
    cp -a /etc/ssh /etc/ssh.cache

EXPOSE 22

RUN mkdir -p /data && chmod 700 /data && cd /data && git init

COPY scripts/entry.sh /entry.sh

ENTRYPOINT ["/entry.sh"]
