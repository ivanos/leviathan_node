# Leviathan Node

This is an Erlang node that contains:

1. [erl_sshd](https://github.com/ivanos/erl_sshd)
1. [dobby_core_lib](https://github.com/ivanos/dobby_core_lib)
2. [dobby_rest_lib](https://github.com/ivanos/dobby_rest_lib)
3. [dobby_ui_lib](https://github.com/ivanos/dobby_ui_lib)
1. [lucet_core_lib](https://github.com/ivanos/lucet_core_lib)
1. [leviathan_lib](https://github.com/ivanos/leviathan_lib)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Leviathan Node](#leviathan-node)
    - [Requirements](#requirements)
    - [Building](#building)
    - [Running](#running)
    - [Connecting via ssh](#connecting-via-ssh)

<!-- markdown-toc end -->


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
