### Leviathan 0.7 Release nodes
The examples listed in the DockerHub documentation.  uses the CEN JSON format with the CIN API calls.  This is correct for this release, but expect the JSON format for CINs to be slightly different in the next release. For example the use of ```cinID``` rather than ```cenID```.

Leviathan will create /16 CINs with 65511 usable IP addresses per CIN.  Leviathan will assign an IP address in the form: ```10.X.Y.Z``` where ```X``` is in the range 7-250 and ```Y.Z```  is in the range 0.10-255.240

```X``` will increment for every CIN and ```Y.Z``` will increment for every container within a CIN.  

For example, the first container in the first CIN will have the IP address: ```10.7.0.10```.

By default, Leviathan will *NOT* set up a CIN to forward to a different network.  But a multi-homed container (e.g. a container which is a member of more than one CIN) will be able to reach containers in every CIN it is a member of.
