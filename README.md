# Leviathan Node

This is an Erlang node that contains:

1. [erl_sshd](https://github.com/ivanos/erl_sshd)
1. [dobby_core_lib](https://github.com/ivanos/dobby_core_lib)
2. [dobby_rest_lib](https://github.com/ivanos/dobby_rest_lib)
3. [dobby_ui_lib](https://github.com/ivanos/dobby_ui_lib)
1. [lucet_core_lib](https://github.com/ivanos/lucet_core_lib)
1. [leviathan_lib](https://github.com/ivanos/leviathan_lib)
1. [erl_cowboy](https://github.com/ivanos/erl_cowboy)



<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Leviathan Node](#leviathan-node)
    - [Requirements](#requirements)
    - [Building](#building)
    - [Running](#running)
    - [Connecting via ssh](#connecting-via-ssh)
    - [websocket log viewer](#websocket-log-viewer)
    - [Configuring TLS](#configuring-tls)
    - [Example](#example)
    - [Multi-host version](#multi-host-version)

## Requirements
- Erlang R17+
- [npm](https://www.npmjs.com/)

## Building
To build the application call: `make`. This will:

* download the dependencies,
* compile the code,
* copy the `dobby_ui_lib` nodeJS artefact to the `dobby_rest/priv/static/www`
* generate an Erlang release,
* generate ssh keys
  * private and public user keys: `ida_rsa` and `id_rsa.pub` respectively
  in the project root directory
  * public and private system keys in the `priv/erl_sshd` that will be used
  by the ssh deamon running in the Erlang node
  * `authorized_keys` file that will already have the previously generated
  user public key

You may add your own public keys to the `authorized_keys` file in
`priv/erl_sshd` (remember to `make rel` afterwards).

If you want to connect to the dobby Erlang shell using ssh with
a username and password, add or modify the usernames and passwords
to the `erl_sshd` section of `config/sys.config`.

## Running

```
_rel/leviathan/bin/leviathan
```

To access the Dobby Visualizer go to http://localhost:8080/static/www/index.html

## Connecting via ssh
If you genereated keys in erl_sshd before generating the dobby release,
you can connect to the dobby server's Erlang shell using ssh.
```
ssh 127.0.0.1 -p 11155 -i id_rsa
```

To exit the Erlang shell obtained via ssh call `exit().`

## websocket log viewer
Logs are published to websocket clients connecting to `ws://.../lager/websocket`.  A simple viewer is provided at `http://.../lager/static/www/index.html`.

## Configuring TLS

To enable TLS support for the HTTP interface you have to configure it in the `erl_cowboy`
application and provide the following options:

* certificate file name (expected in the `priv/erl_cowboy`)
* key file name (expected in the `priv/erl_cowboy`)
* password to the key if it is password protected

The configuration has to be placed in the sys.config file. Below is an example:
```erlang
[
...
 {erl_cowboy, [
               {port, 8080},
               {listeners, 10},
               {app, leviathan},
               {tls_enabled, true},
               {tls_opts, [{certfile, "dummy.crt"},
                           {keyfile, "dummy.key"},
                           {password, ""}]}
               ]},
...
]
```

There is a sample certificate and key generator that you can run with:
`make tls`.
The above example config works with the generated files. To test the TLS,
put the config snippet into the `config/sys.config`. Remember
to re-generated the release after the change.

With TLS enabled, the Visualizer can be accessed via https://localhost:8080/static/www/index.html.

## Example
An example usage of Leviathan is described in the [example](docs/example.org) file.

## Multi-host version

There is a multi-host version of Leviathan on the [multi-host branch](https://github.com/ivanos/leviathan_node/tree/multi-host).
