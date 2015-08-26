# Leviathan 0.8 Release notes

## New in 0.8

* Incremental container membership via ```docker run``` environment variable ```LEV_CIN```

To add Docker container to a Leviathian Conatiner IP Network (CIN), set the container environment variable ```LEV_CIN``` with ```LEV_CIN=<cin1>,<cin2>,...,<cinN>```.  You must also set ```--net=none```.

For example:

```
docker run --net=none -i -t -e LEV_CIN=cen1 ubuntu:14.04 /bin/bash
```

**WARNING:** The CIN must already have been created.  

You can do this using the instructions here:  
[https://hub.docker.com/r/ivanos/leviathan/](https://hub.docker.com/r/ivanos/leviathan/)


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
