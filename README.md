# Leviathan Node

## NOTE: THE MULTI-HOST VERSION IS NOT COMLETE. SEE [NOTES ON THE CURRENT STATE](#notes-on-the-current-state) FOR MORE INFO.

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

## Examples
Examples of Leviathan usage can be found in [docs](docs/).

## Notes on the current state

See the [issues list](https://github.com/ivanos/leviathan_node/issues).

### The demonstration ###

The demonstration of multi-host version of this project can be found [here](https://github.com/ivanos/leviathan_node/blob/multi-host/docs/multi_host_example.org).

To run the demonstration checkout the `multi-host-demo` tag of `multi-host` branch. The following dependencies will be checked out on this tag too when building the project:

1. [leviathan_lib](https://github.com/ivanos/leviathan_lib/tree/multi-host)
1. [leviathan_rest_lib](https://github.com/ivanos/leviathan_rest_lib/tree/multi-host)
1. [dobby_oflib](https://github.com/ivanos/dobby_oflib/tree/multi-host)

The easiest way of running the demo is by using the Vagrant project [Leviathan-Cluster](https://github.com/mentels/Leviathan-Cluster) that builds appropriate environment automatically. The Vagrant environment relies on the Docker images from [this repository](https://hub.docker.com/r/mentels/dockerfiles/), which in turn build the images based on the Dockerfiles stored in [this repository](https://github.com/ivanos/dockerfiles/tree/multi-host).

### Requirements to build Leviathan Cluster across several hosts

1. The hostnames have to be configured in the hosts `/etc/hosts`.
2. The Leviathan container has to be started like below:

    ```bash
    docker run -v /run:/run -v /var:/host/var -v /proc:/host/proc --net=host --privileged=true -itd --name leviathan mentels/dockerfiles:leviathan-multi-host-demo
    ````

   The Leviathan built with this Docker image assumes that the master Leviathan node (that has to be started first) is `leviathan@leviathan1` and is running on host `leviathan1`. It also assumes that `vagrant` is the user-name that can invoke sudo without password (it is required for creating the tunnels).

   If these values have to be different, new Docker image with Leviathan has to be built with appropriate configuration provided in [overlay_vars.config](https://github.com/ivanos/leviathan_node/blob/multi-host/config/overlay_vars.config).


3. The hosts that take part in Leviathan cluster have to be able to create ssh tunnels between them. To achieve that the following requirements have to be fulfilled:

   * the user that set ups tunnels needs the password-less sudo or has to be privileged (the `tunnel_user`)
   * ssh server has to allow for creating tunnels (`PermitTunnel` option set to `yes` in `/etc/ssh/sshd`; the ssh has to be reloaded after changing a configuration option). 
   * key authentication for the `tunnel_user` has to be enabled between the hosts
   * IP forwarding has to be enabled
       * allow the forwarding for the current session
         `sysctl -w net.ipv4.ip_forward=1`
       * make the setting persistent by adding  the following to the `/etc/sysctl.conf`
         `net.ipv4.ip_forward=1`


### Desing assumptions ###

1. The whole `LMs` (Leviathan Map) are pushed to the nodes in the cluster. There's no mnesia replication. In the future the `LMs` could be filtered based on the responsibility of the particular nodes (e.g., `cen1` may span only `Host1` and `Host2` and `Host3` does not need information on it).
2. Currently clustering is based on "master node" and other Leviathan nodes connect to it (`{master_node, NodeName}` option in the [sys.config](https://github.com/ivanos/leviathan_node/blob/multi-host/config/sys.config) based on the [overlay_vars.config](https://github.com/ivanos/leviathan_node/blob/multi-host/config/overlay_vars.config)). Possible future solutions are:

   * Dobby stores information on the cluster by maintaining "well-known" identifier with relevant data
   * any Leviathan node can connect to any other
   
1. Dobby runs on a separate node that also is part of the cluster and is accessible as a global process
2. Each Leviathan node runs FSMs for CENs (a CEN FSM on each host the CEN spans over):

    - there is a CEN FSM (`lev_cen_fsm` module) on each host the CEN spans over
    - the FSM invokes leviathan_lib based on the changes of CEN status
    - the FSM subscribes for status changes of CEN identifier and sends message via it
    - the Exectuive (`lev_executive` module) instance that handles a particular request updates the CEN status
    - only the Executive that handles a request on a particular host is invoked
        - leviathan_lib knows nothing about the distribution
          - it does not run RPCs
          - it does not know, that we use Erlang Distributed
          - it knows that different network elements run on different hosts

3. Each Leviathan node runs FSMs for CINs (a CIN FSM on each host the CIN spans over):
   Assumptions are the same as for the CEN FSMs (`lev_cin_fsm` module).

### Useful commands

* Running Leviathan node for testing purposes:

  ```bash
  erl -pa ../leviathan/ebin -pa deps/*/ebin -sname ala -erl_mnesia options \[persistent\]  -leviathan_lib docker_bin cat -config config/sys -eval "application:ensure_all_started(leviathan)"
  ```

* Checking if there are any tunnels in the system:
  `pgrep -fal Tunnel=Ethernet`

* Creating a tunnel between two hosts:

   1. Setting up the tap interfaces (on both hosts):

      ```bash
      sudo ip tuntap add dev tap102 mode tap
      sudo ip link set dev tap102 up
       ```

   2. Creating a tunnel between `tap102` on "this host" and `tap102` on `leviathan2`
      `sudo -u vagrant ssh -f -o Tunnel=Ethernet -w 102:102 vagrant@leviathan2 true`

   3. Killing the tunnels
      `pkill -f Tunnel=Ethernet`

### Sequence diagrams

#### [/cen/import](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vaW1wb3J0CgCCKAUtPiBFWDogAA0GIENFTnMAgQkIQ0VOOiBjb25zdHJ1Y3QgQ2VuTE0KQ0VOIC0tPgAvBgANBgCBOgU-IERNOiBwdWJsaXNoACYGIGFuZCBtYXJrIGFzICIAbgZpbmciAIFpBwCCAgUAbwoALQUAgUkHc3RvcmVzIGFjcm9zAIFJBmNsdXN0ZXIAgiQIRE06IHN1YnNjcmliZSBmb3IgbWVzc2FnZXMgZnJvbSBGU01zIHZpYQCDCAVpZGVudGlmaWVycwpub3RlIHJpZ2h0IG9mAIF0BUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNFTgBKBTt3aGUAgmgGQ0VOc1xuYXJlIGNvcnJlY3RseQCCOAcAgxIFIGFsbCBob3N0c1xuACgHIElEcyBhcmUAggkFZWRcbmFzICJwZW5kAIIKBQCDeQdGU006IHN0YQCDAAYAgT0GAIEdE2FuAIUXBWZvciBlYWNoAIRqBW9uAAYGaG9zdABrCnNwYW5zIG92ZXIKCmVuZAoKRlNNAIInFnN0YXR1c1xuY2hhbmdlIG9mAIRQBmV3AIQKBgAsDGVuZACCZAggYnkAgVcGZFxuaW5kaWNhdGluZwCFAgZ0aGUAhj4FaXMgcmVhZHkAgmoPAIFnBQAdB3Mgd2FudFxudG8gc2VlAIN5B2F0dXMgdHJhbnNpc3Rpb25cbm9mAIUYBQCDWgYAhEQJIHRvIACCQgcAhWo-AIZ-B0VYOiB3YWl0AIRHDVxuAIRDCElkAIQPBwCBNAgAhQQMZQCGOQYAgTUJbwCDVBAtPgCHXwcAhnMHLT4-IENMOgCDGQU&s=roundgreen)

![/cen/import](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vaW1wb3J0CgCCKAUtPiBFWDogAA0GIENFTnMAgQkIQ0VOOiBjb25zdHJ1Y3QgQ2VuTE0KQ0VOIC0tPgAvBgANBgCBOgU-IERNOiBwdWJsaXNoACYGIGFuZCBtYXJrIGFzICIAbgZpbmciAIFpBwCCAgUAbwoALQUAgUkHc3RvcmVzIGFjcm9zAIFJBmNsdXN0ZXIAgiQIRE06IHN1YnNjcmliZSBmb3IgbWVzc2FnZXMgZnJvbSBGU01zIHZpYQCDCAVpZGVudGlmaWVycwpub3RlIHJpZ2h0IG9mAIF0BUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNFTgBKBTt3aGUAgmgGQ0VOc1xuYXJlIGNvcnJlY3RseQCCOAcAgxIFIGFsbCBob3N0c1xuACgHIElEcyBhcmUAggkFZWRcbmFzICJwZW5kAIIKBQCDeQdGU006IHN0YQCDAAYAgT0GAIEdE2FuAIUXBWZvciBlYWNoAIRqBW9uAAYGaG9zdABrCnNwYW5zIG92ZXIKCmVuZAoKRlNNAIInFnN0YXR1c1xuY2hhbmdlIG9mAIRQBmV3AIQKBgAsDGVuZACCZAggYnkAgVcGZFxuaW5kaWNhdGluZwCFAgZ0aGUAhj4FaXMgcmVhZHkAgmoPAIFnBQAdB3Mgd2FudFxudG8gc2VlAIN5B2F0dXMgdHJhbnNpc3Rpb25cbm9mAIUYBQCDWgYAhEQJIHRvIACCQgcAhWo-AIZ-B0VYOiB3YWl0AIRHDVxuAIRDCElkAIQPBwCBNAgAhQQMZQCGOQYAgTUJbwCDVBAtPgCHXwcAhnMHLT4-IENMOgCDGQU&s=roundgreen)

#### [/cen/make](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vbWFrZQoAgiYFLT4gRVg6IG1ha2UgQ0VOcwCBBQhETTogc3Vic2NyaWJlIGZvciBtZXNzYWdlcyBmcm9tIEZTTXMgdmlhAIFpBWlkZW50aWZpZXJzCm5vdGUgcmlnaHQgb2YAVwVFWCBjb2xsZWN0cyBjb25maXJtYXRpb25zXG4ARgVDRU4ASgU7IHdoZQCBSgZDRU5zXG5hcmUgY29ycmVjdGx5ICJwcmVwYXJlZCIgb24gYWxsIGhvc3RzXG4AKgcgSURzIGFyZSBtYXJrZWRcbmFzICJyZWFkeSIAgUkMbWFyawBbCSBhcwBQCGluZyIKCmVuZAoKRE0gLT4gRlNNOiBub3RpZnkgYWJvdXQgc3RhdHVzIGNoYW5nZVxuIHRvADANRlMAMwVDRU46IACBJQcAgUQJABUIAIJSBWVuZACCRgggYnkAgTYGZFxuaW5kaWNhdGluZwCDRQZ0aGUAhQEFAIFwCACBYApvAINwBmhvc3QAg18-AIRzB0VYOiB3YWl0AINbDVxuAINXCElkAIMjBwCBCwdzAIQYDGUAhisFcwCCFQh0bwCCbAlFWCAtLT4AhVEHAIRnBy0-PiBDTDoAgmQF&s=roundgreen)

![/cen/make](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vbWFrZQoAgiYFLT4gRVg6IG1ha2UgQ0VOcwCBBQhETTogc3Vic2NyaWJlIGZvciBtZXNzYWdlcyBmcm9tIEZTTXMgdmlhAIFpBWlkZW50aWZpZXJzCm5vdGUgcmlnaHQgb2YAVwVFWCBjb2xsZWN0cyBjb25maXJtYXRpb25zXG4ARgVDRU4ASgU7IHdoZQCBSgZDRU5zXG5hcmUgY29ycmVjdGx5ICJwcmVwYXJlZCIgb24gYWxsIGhvc3RzXG4AKgcgSURzIGFyZSBtYXJrZWRcbmFzICJyZWFkeSIAgUkMbWFyawBbCSBhcwBQCGluZyIKCmVuZAoKRE0gLT4gRlNNOiBub3RpZnkgYWJvdXQgc3RhdHVzIGNoYW5nZVxuIHRvADANRlMAMwVDRU46IACBJQcAgUQJABUIAIJSBWVuZACCRgggYnkAgTYGZFxuaW5kaWNhdGluZwCDRQZ0aGUAhQEFAIFwCACBYApvAINwBmhvc3QAg18-AIRzB0VYOiB3YWl0AINbDVxuAINXCElkAIMjBwCBCwdzAIQYDGUAhisFcwCCFQh0bwCCbAlFWCAtLT4AhVEHAIRnBy0-PiBDTDoAgmQF&s=roundgreen)

#### [/cen/destroy](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vZGVzdHJveQoAgikFLT4gRVg6IAANByBDRU5zAIELCERNOiBzdWJzY3JpYmUgZm9yIG1lc3NhZ2VzIGZyb20gRlNNcyB2aWEAgW8FaWRlbnRpZmllcnMKbm90ZSByaWdodCBvZgBaBUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNFTgBKBTsgd2hlAIFQBkNFTnNcbmFyZSBjb3JyZWN0bHkgIgCBNAdlZCIgb24gYWxsIGhvc3RzXG4AKwcgSURzIGFyZSBtYXJrZWRcbmFzICJwZW5kaW5nIgCBTAxtYXJrAF4JIGFzAFIJACMGZW5kCgpETSAtPiBGU006IG5vdGlmeSBhYm91dCBzdGF0dXMgY2hhbmdlXG4gdG8AMA5GUwA0BUNFTgCCTgoAgUoIABUIAIJXBWVuZACCSwggYgCCdwUgSWRcbmluZGljYXRpbmcAg1AGdGhlAIUMBQCBdAkAgWUKbwCDfAZob3N0AINrPgCEfwdFWDogd2FpdACDYQ1cbgCDXQhJZACDKQcAgQwHcwCEHgxlAIY3BXMAghcIdG8Agm8LRVggLS0-AIVfBwCEcgctPj4gQ0w6AIJoBQ&s=roundgreen)

![/cen/destroy](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DRU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DRU4gTG9naWMAgRoFRU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CiAgIApDTABWCi9jZW4vZGVzdHJveQoAgikFLT4gRVg6IAANByBDRU5zAIELCERNOiBzdWJzY3JpYmUgZm9yIG1lc3NhZ2VzIGZyb20gRlNNcyB2aWEAgW8FaWRlbnRpZmllcnMKbm90ZSByaWdodCBvZgBaBUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNFTgBKBTsgd2hlAIFQBkNFTnNcbmFyZSBjb3JyZWN0bHkgIgCBNAdlZCIgb24gYWxsIGhvc3RzXG4AKwcgSURzIGFyZSBtYXJrZWRcbmFzICJwZW5kaW5nIgCBTAxtYXJrAF4JIGFzAFIJACMGZW5kCgpETSAtPiBGU006IG5vdGlmeSBhYm91dCBzdGF0dXMgY2hhbmdlXG4gdG8AMA5GUwA0BUNFTgCCTgoAgUoIABUIAIJXBWVuZACCSwggYgCCdwUgSWRcbmluZGljYXRpbmcAg1AGdGhlAIUMBQCBdAkAgWUKbwCDfAZob3N0AINrPgCEfwdFWDogd2FpdACDYQ1cbgCDXQhJZACDKQcAgQwHcwCEHgxlAIY3BXMAghcIdG8Agm8LRVggLS0-AIVfBwCEcgctPj4gQ0w6AIJoBQ&s=roundgreen)


#### [/cin/import](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vaW1wb3J0CgCCJQUtPiBFWDogAA0GIENJTnMAgQYIQ0lOOiBjb25zdHJ1Y3QgQ2luTE0KQ0lOIC0tPgAvBgANBgCBNwU-IERNOiBwdWJsaXNoACYGIGFuZCBtYXJrIGFzICIAbgZpbmciAIFmBwCBfwUAbwoALQUAgUYHc3RvcmVzIGFjcm9zAIFGBmNsdXN0ZXIAgiEIRE06IHN1YnNjcmliZSBmb3IgbWVzc2FnZXMgZnJvbSBGU01zIHZpYQCDBQVpZGVudGlmaWVycwpub3RlIHJpZ2h0IG9mAIF0BUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNJTgBKBTt3aGUAgmUGQ0lOc1xuYXJlIGNvcnJlY3RseQCCOAcAgw8FIGFsbCBob3N0c1xuACgHIElEcyBhcmUAggkFZWRcbmFzICJwZW5kAIIKBQCDdgdGU006IHN0YQCDAAYAgT0GAIEdE2FuAIUUBWZvciBlYWNoAIRnBW9uAAYGaG9zdABrCnNwYW5zIG92ZXIKCmVuZAoKRlNNAIInFnN0YXR1c1xuY2hhbmdlIG9mAIRNBmV3AIQKBgAsDGVuZACCZAggYnkAgVcGZFxuaW5kaWNhdGluZwCEfwZ0aGUAhjsFaXMgcmVhZHkAgmoPAIFnBQAdB3Mgd2FudFxudG8gc2VlAIN5B2F0dXMgdHJhbnNpc3Rpb25cbm9mAIUYBQCDWgYAhEQJIHRvIACCQgcAhWY_AIZ8BkVYOiB3YWl0AIRHDVxuAIRDCElkAIQPBwCBNAgAhQQMZQCGOQYAgTUJbwCDVBAtPgCHXAcAhnMHLT4-IENMOgCDFwc&s=roundgreen)

![/cin/import](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vaW1wb3J0CgCCJQUtPiBFWDogAA0GIENJTnMAgQYIQ0lOOiBjb25zdHJ1Y3QgQ2luTE0KQ0lOIC0tPgAvBgANBgCBNwU-IERNOiBwdWJsaXNoACYGIGFuZCBtYXJrIGFzICIAbgZpbmciAIFmBwCBfwUAbwoALQUAgUYHc3RvcmVzIGFjcm9zAIFGBmNsdXN0ZXIAgiEIRE06IHN1YnNjcmliZSBmb3IgbWVzc2FnZXMgZnJvbSBGU01zIHZpYQCDBQVpZGVudGlmaWVycwpub3RlIHJpZ2h0IG9mAIF0BUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNJTgBKBTt3aGUAgmUGQ0lOc1xuYXJlIGNvcnJlY3RseQCCOAcAgw8FIGFsbCBob3N0c1xuACgHIElEcyBhcmUAggkFZWRcbmFzICJwZW5kAIIKBQCDdgdGU006IHN0YQCDAAYAgT0GAIEdE2FuAIUUBWZvciBlYWNoAIRnBW9uAAYGaG9zdABrCnNwYW5zIG92ZXIKCmVuZAoKRlNNAIInFnN0YXR1c1xuY2hhbmdlIG9mAIRNBmV3AIQKBgAsDGVuZACCZAggYnkAgVcGZFxuaW5kaWNhdGluZwCEfwZ0aGUAhjsFaXMgcmVhZHkAgmoPAIFnBQAdB3Mgd2FudFxudG8gc2VlAIN5B2F0dXMgdHJhbnNpc3Rpb25cbm9mAIUYBQCDWgYAhEQJIHRvIACCQgcAhWY_AIZ8BkVYOiB3YWl0AIRHDVxuAIRDCElkAIQPBwCBNAgAhQQMZQCGOQYAgTUJbwCDVBAtPgCHXAcAhnMHLT4-IENMOgCDFwc&s=roundgreen)

#### [/cin/make](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vbWFrZQoAgiMFLT4gRVg6IG1ha2UgQ0lOcwCBAghETTogc3Vic2NyaWJlIGZvciBtZXNzYWdlcyBmcm9tIEZTTXMgdmlhAIFmBWlkZW50aWZpZXJzCm5vdGUgcmlnaHQgb2YAVwVFWCBjb2xsZWN0cyBjb25maXJtYXRpb25zXG4ARgVDSU4ASgU7IHdoZQCBRwZDSU5zXG5hcmUgY29ycmVjdGx5ICJwcmVwYXJlZCIgb24gYWxsIGhvc3RzXG4AKgcgSURzIGFyZSBtYXJrZWRcbmFzICJyZWFkeSIAgUkMbWFyawBbCSBhcwBQCGluZyIKCmVuZAoKRE0gLT4gRlNNOiBub3RpZnkgYWJvdXQgc3RhdHVzIGNoYW5nZVxuIHRvADANRlMAMwVDSU46IACBJQcAgUQJABUIAIJSBWVuZACCRgggYnkAgTYGZFxuaW5kaWNhdGluZwCDQgZ0aGUAhH4FAIFwCACBYApvAINtBmhvc3QAg1s_AIRxBkVYOiB3YWl0AINbDVxuAINXCElkAIMjBwCBCwdzAIQYDGUAhigFcwCCFQh0bwCCbAlFWCAtLT4AhU4HAIRnBy0-PiBDTDoAgmQF&s=roundgreen)

![/cin/make](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vbWFrZQoAgiMFLT4gRVg6IG1ha2UgQ0lOcwCBAghETTogc3Vic2NyaWJlIGZvciBtZXNzYWdlcyBmcm9tIEZTTXMgdmlhAIFmBWlkZW50aWZpZXJzCm5vdGUgcmlnaHQgb2YAVwVFWCBjb2xsZWN0cyBjb25maXJtYXRpb25zXG4ARgVDSU4ASgU7IHdoZQCBRwZDSU5zXG5hcmUgY29ycmVjdGx5ICJwcmVwYXJlZCIgb24gYWxsIGhvc3RzXG4AKgcgSURzIGFyZSBtYXJrZWRcbmFzICJyZWFkeSIAgUkMbWFyawBbCSBhcwBQCGluZyIKCmVuZAoKRE0gLT4gRlNNOiBub3RpZnkgYWJvdXQgc3RhdHVzIGNoYW5nZVxuIHRvADANRlMAMwVDSU46IACBJQcAgUQJABUIAIJSBWVuZACCRgggYnkAgTYGZFxuaW5kaWNhdGluZwCDQgZ0aGUAhH4FAIFwCACBYApvAINtBmhvc3QAg1s_AIRxBkVYOiB3YWl0AINbDVxuAINXCElkAIMjBwCBCwdzAIQYDGUAhigFcwCCFQh0bwCCbAlFWCAtLT4AhU4HAIRnBy0-PiBDTDoAgmQF&s=roundgreen)

#### [/cin/destroy](https://www.websequencediagrams.com/?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vZGVzdHJveQoAgiYFLT4gRVg6IAANByBDSU5zAIEICERNOiBzdWJzY3JpYmUgZm9yIG1lc3NhZ2VzIGZyb20gRlNNcyB2aWEAgWwFaWRlbnRpZmllcnMKbm90ZSByaWdodCBvZgBaBUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNJTgBKBTsgd2hlAIFNBkNJTnNcbmFyZSBjb3JyZWN0bHkgIgCBNAdlZCIgb24gYWxsIGhvc3RzXG4AKwcgSURzIGFyZSBtYXJrZWRcbmFzICJwZW5kaW5nIgCBTAxtYXJrAF4JIGFzAFIJACMGZW5kCgpETSAtPiBGU006IG5vdGlmeSBhYm91dCBzdGF0dXMgY2hhbmdlXG4gdG8AMA5GUwA0BUNJTgCCTgoAgUoIABUIAIJXBWVuZACCSwggYgCCdwUgSWRcbmluZGljYXRpbmcAg00GdGhlAIUJBQCBdAkAgWUKbwCDeQZob3N0AINnPwCEfQZFWDogd2FpdACDYQ1cbgCDXQhJZACDKQcAgQwHcwCEHgxlAIY0BXMAghcIdG8Agm8LRVggLS0-AIVcBwCEcgctPj4gQ0w6AIJoBQ&s=roundgreen)

![/cin/destroy](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=cGFydGljaXBhbnQgQ2xpZW50IGFzIENMCgANDFJFU1QgQVBJIGFzAAcFABANTGV2aWF0aGFuXG5FeGVjdXRpdmUgYXMgRVgANw1DSU5cIEZTTSBhcyBGU00AUw1Eb2JieVxuKFNlcnZpY2UpIGFzIEQAFQ5DSU4gTG9naWMAgRoFSU4ADBFTdG9yAHMFAAQFCgoKRVggLT4AgTYFOiByZWdpc3RlciBjYWxsYmFja3MKCm9wdCB0aGlzIGlzIGUAgTQFZWQgb25seSBpbiB0aGUgbm9kZSB0aGF0IGdldHMADgVyZXF1ZXN0CgpDTABTCi9jaW4vZGVzdHJveQoAgiYFLT4gRVg6IAANByBDSU5zAIEICERNOiBzdWJzY3JpYmUgZm9yIG1lc3NhZ2VzIGZyb20gRlNNcyB2aWEAgWwFaWRlbnRpZmllcnMKbm90ZSByaWdodCBvZgBaBUVYIGNvbGxlY3RzIGNvbmZpcm1hdGlvbnNcbgBGBUNJTgBKBTsgd2hlAIFNBkNJTnNcbmFyZSBjb3JyZWN0bHkgIgCBNAdlZCIgb24gYWxsIGhvc3RzXG4AKwcgSURzIGFyZSBtYXJrZWRcbmFzICJwZW5kaW5nIgCBTAxtYXJrAF4JIGFzAFIJACMGZW5kCgpETSAtPiBGU006IG5vdGlmeSBhYm91dCBzdGF0dXMgY2hhbmdlXG4gdG8AMA5GUwA0BUNJTgCCTgoAgUoIABUIAIJXBWVuZACCSwggYgCCdwUgSWRcbmluZGljYXRpbmcAg00GdGhlAIUJBQCBdAkAgWUKbwCDeQZob3N0AINnPwCEfQZFWDogd2FpdACDYQ1cbgCDXQhJZACDKQcAgQwHcwCEHgxlAIY0BXMAghcIdG8Agm8LRVggLS0-AIVcBwCEcgctPj4gQ0w6AIJoBQ&s=roundgreen)
