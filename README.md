## Dog

A loyal and faithful synchronisation tool that you can rely on.

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

### Structure

On the client side, the current branch is the client name:

```
[client] $ git branch
* <NAME-1>
[client] $ tree
...
```

On the server side, every client state is stored in a subtree of the master
branch:

```
[server]$ git branch
* master
[server] $ tree
<NAME-1>/...
<NAME-2>/...
```
