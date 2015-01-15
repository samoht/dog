## Dog

A loyal and faithful synchronisation tool that you can rely on.

### The `.merge` file

Dog client and server might have a `.merge` file at the root of their
repository.

```
<FILE-PATTERN-1> <MERGE-STRATEGY-1>
<FILE-PATTERN-2> <MERGE-STRATEGY-2>
...
```

- The `<FILE-PATTERN-n>` are (globs) regular expression identifying
filenames in the repository. The pattern are tested in order to find
which merge stategy to use on the server-side.

- `<MERGE-STRATEGY-n>` is the merge strategy used by the server to merge
the client files. The supported strategies are:

  - `ignore` (default): Ignore the file.

  - `unique`: Ensure that the client's file does not already exist on
  the server.

  - `set`: Add the set of client's file lines into the server file.

  - `append`: Append the client's new lines to the server file. The
  common file prefix between the client and the server is not
  duplicated.

  - `jsonx`: Consider the file as a JSON value and merge the value on
  the server with the client version. The `x` stands for the special
  merge semantics that we use: records are considered as k/v maps and
  are merged as follow: if the key exists on both the client and the
  server, then merge the values together -- otherwise always add new
  client keys. Arrays are considered as unordered sets. (FIXME ?)


### Client Commands

#### Init

Init the client.

```
# init the client
dog init <NAME>
```

- `<NAME>` is the client name. It is is used to identify the client in
  an unique way for both the server's branch name mirroring the client's
  master and for the client's commit username.

#### Push

Send data to the server. The error code indicate server merge success or error.

```
dog push -m "<MSG>" <SERVER>
```

- <MSG> is the commit message.
- `<SERVER>` is the server url.

### Server commands

#### Listen

Start the server and listen for incoming clients.

```
dog listen
```
