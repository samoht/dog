## Dog

A loyal and faithful synchronisation tool that you can rely on.

### Client-side

#### The `.dog` file

Dog's client manage a Git repository containing at least a `.dog` file
at its root. The format of the `.dog` file is:

```
client="<WORKER>"
server="<SERVER>"
merges=[
  [ "<FILE-PATTERN>", "<MERGE-STRATEGY>" ];
  [ "<FILE-PATTERN>", "<MERGE-STRATEGY>" ];
  ...
]
```

`<CLIENT>` is the client name. It is is used to identify the client in
an unique way for both the server's branch name mirroring the client's
master and for the client's commit username.

`<SERVER>` is the server url.

The `<FILE-PATTERN>` are (globs) regular expression identifying
filenames in the repository. The pattern are tested in order to find
which merge stategy to use on the server-side.

`<MERGE-STRATEGY>` is the merge strategy used by the server to merge
the client files. The supported strategies are:

- `ignore` (default): Ignore the file.

- `replace`: Replace the server file by the client one.

- `line-set`: Add the set of client's file lines into the server file.

- `line-append`: Append the client's new lines to the server file. The
  common file prefix between the client and the server is not
  duplicated.

- `jsonx`: Consider the file as a JSON value and merge the value on
  the server with the client version. The `x` stands for the special
  merge semantics that we use: records are considered as k/v maps and
  are merged as follow: if the key exists on both the client and the
  server, then merge the values together -- otherwise always add new
  client keys. Arrays are considered as unordered sets. (FIXME ?)


#### Commands

```
# init client
dog init <CLIENT> <SERVER>
```

```
# set-up merge strategies. The configuration is done on the client
# but the actual merge is performed by the server
dog merge [list|add|remove] <PATTERN> [ignore|replace|line-set|line-append|jsonx]
```

```
# send data to the server, with a commit message
# The error code indicate server merge success or error
dog push -m "<MSG>"
```

```
# start the server
dog listen
```
