## Dog

A loyal and faithful synchronisation tool that you can rely on.

This simple tool allows to watch distributed directories and gather
the changes in a central Git repository, where every watched directories
appear as sub-directories.

### Example

```
# Machine A, watch the current directory and make it available
# as `<global-name-A>` globally
dog watch <global-name-A> <server-url>

# Machine B, watch the current directory and make it available
# as `<global-name-B>` globally
dog wath <global-name-B> <server-url>
```

The on the server, the Git repository corresponding to `<server-url>`
will have the file hierarchy:
```
<global-name-A>/<files watched on machine A>
<global-name-B>/<files watches on machine B>
...
```

With the full history of changes.

### Running the server

The easiest way to run a dog server is to run:

```
docker run -it --rm -p 22:22 \
  -v <secrets>/id_rsa.pub:/root/.ssh/authorized_keys \
  -v <git-repo>:/data \
  samoht/dogd
```

Or you can use the `dog listen` commands.

### Running the client

```
dog watch --root=<directory-to-watch> <global-name> <ssh-server-url>:/data
```

For one-shot updates, you can use `dog watch --once` or simply:

```
cd <directory-to-watch> && git init && \
  git add * && git commit -a -m "Dog" && \
  git push <ssh-server-url>:/data <global-name> --force
```
