# Leviathan 0.8 Release notes

## New in 0.8

* Incremental container membership via ```docker run``` environment variable ```LEV_CIN```

To add Docker container to a Leviathian Conatiner IP Network (CIN), set the container environment variable ```LEV_CIN``` with ```LEV_CIN=<cin1>,<cin2>,...,<cinN>```.  You must also set ```--net=none```.

For example:

```
docker run --net=none -i -t -e LEV_CIN=cen1 ubuntu:14.04 /bin/bash
```

**WARNING:** The CIN must already have been created. You can do this using the instructions here:  
[https://hub.docker.com/r/ivanos/leviathan/](https://hub.docker.com/r/ivanos/leviathan/)


### Try it! ###

1. Launch Leviathan 0.8
```
docker run -v /run:/run -v /var:/host/var -v /proc:/host/proc  --net=host --privileged=true -i -t ivanos/leviathan:rel-0.8
```
2. Create couple of CINS using the Leviathan test tool. You can find cpool.json used in this example here: [https://github.com/ivanos/leviathan_lib/blob/master/cpool.json](https://github.com/ivanos/leviathan_lib/blob/master/cpool.json)
```
curl -d@cpool.json http://localhost:8080/cpool
```
3. Ping a container (from the host) to make sure everything is working
```
$ ping 10.7.0.10
PING 10.7.0.10 (10.7.0.10) 56(84) bytes of data.
64 bytes from 10.7.0.10: icmp_seq=1 ttl=64 time=0.050 ms
64 bytes from 10.7.0.10: icmp_seq=2 ttl=64 time=0.090 ms
```
4. Start a new container and add it to ```cen2``
```
$ docker run -i --net=none -e LEV_CIN=cen2 -t ubuntu:14.04 /bin/bash
root@34fc181ba62d:/#
```
5. Visually inspect that the container is a member of ```cen2```
```
root@34fc181ba62d:/# ip a
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
89: cen2: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether b2:46:3c:1f:fd:83 brd ff:ff:ff:ff:ff:ff
    inet 10.7.0.16/16 scope global cen2
       valid_lft forever preferred_lft forever
    inet6 fe80::b046:3cff:fe1f:fd83/64 scope link tentative 
       valid_lft forever preferred_lft forever
```
6. ping the container from the host
```
$ ping 10.7.0.16
PING 10.7.0.16 (10.7.0.16) 56(84) bytes of data.
64 bytes from 10.7.0.16: icmp_seq=1 ttl=64 time=0.070 ms
64 bytes from 10.7.0.16: icmp_seq=2 ttl=64 time=0.075 ms
64 bytes from 10.7.0.16: icmp_seq=3 ttl=64 time=0.088 ms
```

## New in 0.7.1: container pools (cpools) for tesing

In order to test Leviathan in isolation (i.e. no requirement for other ochestration tools), 0.7.1 introduces support for container pools (cpools).  By defining a cpool, some number of containers with a specific tag will be started and added to specific container ip networks (CINs).   A RESTful API ```/cpool``` is added to upload a list of cpools, start the containers and wire the network accordingly.  Using this facility the tester does not have to keep track of ContainerIDs.

*UNDER CONSTRUCTION: 0.9* For this release ```/bin/bash``` will be executed in the containers, but that will be settable in 0.9

## API
URI | Method | Body | Description
--- | ------ | ---- | -----------
/cpool | POST | JSON file | upload CPool JSON file

## CPool Structure

```
{
 "cpoolID": <CPoolId>,
 "type": <ContainerType>,
 "start_with": <Number of ContainerType to run>,
 "cins":[{"cinID":<CinID>,"default_route":<optional "true"|"false">},...]
 }
```

## Example CPool JSON File (e.g. ```cpool.json``` )
```
{"cpoolList":
 [
     {"cpoolID":"pool1",
      "type":"ubuntu:14.04",
      "start_with": 3,
      "cins":[{"cinID":"cen1","default_route":"true"},
              {"cinID":"cen2"}]},
     {"cpoolID":"pool2",
      "type":"ubuntu:14.04",
      "start_with": 3,
      "cins":[{"cinID":"cen2"}]}
 ]
}
```
## Example of using ```/cpool```

1. Load this file into Leviathan with the following command:
 ```curl -d @/tmp/cpool.json http://<lev_host>:8080/cpool```
2. Check the host machine and running containers.  The JSON file in this example will create six containers and two bridges ```cen1``` and ```cen2```.  Three (of six) containers will be added  to ``cen1`` and six (of six) containers to  ```cen2```. It will assign IP Addresses of the form ```10.7.X.Y``` to ```cen1``` and ```10.8.X.Y``` to ```cen2```.
You can see the bridges and interfaces  created on the host by running:
```ip a```
You can see the interfaces created in the containers and their IP Addresses by running:
```docker exec <container id> ip a```
5.  Exec into various containers that are members of *the same CIN* and try to have them ping one another
5.  Try to ping containers from the host
6.  Remove the networking components from the host machine and running containers:
```curl -d '["cen1","cen2"]' http://<lev_host>:8080/cin/destroy```

## Other notes:

The examples listed in the DockerHub documentation for 0.7.1 [https://hub.docker.com/r/ivanos/leviathan/](https://hub.docker.com/r/ivanos/leviathan/) uses the CEN JSON format with the CIN API calls.  This is correct for this release, but expect the JSON format for CINs to be slightly different in the next release. For example the use of ```cinID``` rather than ```cenID```.

Leviathan will create /16 CINs with 65511 usable IP addresses per CIN.  Leviathan will assign an IP address in the form: ```10.X.Y.Z``` where ```X``` is in the range 7-250 and ```Y.Z```  is in the range 0.10-255.240

```X``` will increment for every CIN and ```Y.Z``` will increment for every container within a CIN.  

For example, the first container in the first CIN will have the IP address: ```10.7.0.10```.

Leviathan will always set up CINs to forward to a different networks using a ```default gateway``` of ```10.X.0.1```. Multi-homed containers (e.g. containers which are members of more than one CIN) will be able to reach containers in every CIN it is a member directly through the membership interface and not through the ```default gateway```.
