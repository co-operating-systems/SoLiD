
Every `ldp:Resource` has at least one managed resource: the acl for that resource.  So an `ldp:Container` and each of its `ldp:contains` resources has an associated acl. 
                        
### Difference between Client and Server Managed Resources

The ACR associated with a container acts like an ldp:Resource, not like an ldp:Container: so there the difference is clearly big enough that the actors implementing them need to be distinguished. 

What are the differences between an ACR and a normal RDF Resource, a.k.a. an `ldp:RDFSource`?
* ACRs are not ldp:Resources (so no `Link: <...ldp#Resource>; rel="type"` header)
  (on the other hand there may be a special `Link: <.acl>; rel="acl"` header linking back to itself.)
* Life cycle:
  * ACRs are not created with a POST on a container. They are created when another resource gets created. 
  (Could this be modelled as a POST on the resource thought of as a special type of SM LDPC? -- but beware confusion with POST as append)
  * DELETEing the resource created with a POST will delete the associated SM ACR too,
  * There is no special archive for the acl, it goes into an archive with all the other content
* ACL's need only allow a restricted set of RDF content, as they are so essential to the functioning of the system. 
  Such restrictions could be imposed generically by ShEx or ShACLs, in which case this difference could be part of a generic feature set
* An ACR should be able to return an NQuads representation containing all the `:imports` as per [issue 210](https://github.com/solid/authorization-panel/issues/210#issuecomment-838747077). 
  This means that it needs some extra behavior in addition to a simple resource actor for GET requests. 

Finally: should the ACR Actor play the role of the Guard? That would constitute a reasonably serious difference. On the other hand the Guard is perhaps just a Script that transforms a WannaDo into a Do.
                          
### Is a reduction possible?
   
Two Questions:
1. Can one extract a core resource behavior?
2. Could one build all behavior on top of this using scripts?

* For POST: 
   Could one see the creation of SMR's (Server Managed Resources) as the server (via Scripts) doing a POST on the created resource (seen as a directory (hence the `.`)) to create those resources. (only ACLs for the moment)
* For DELETE:
  Only the server is allowed to use that method on SMR.
  This still leaves the difference in how archivage is treated. 
  But perhaps that is because we used a directory archiving strategy. 
We could also add an attribute "Deleted" to each resource and that be enough to make it count as deleted! 
Then there would be no difference here - the general principle being that if an intermediary resource is deleted all resources downstream of the path become inaccessible.

Could we state the difference then simply in terms of who owns the resource, and so who controls them?
* Client managed resources are controlled by the agent that created them 
* Server created resources are controlled by the server - hence clients cannot use POST or DELETE methods

That seems like it could be expressed in terms of (potentially implicit) Access Control Rules? 
Perhaps it is that the server has an access control rule that allows it to create and delete any resource. 
If the client can sets access control rules on access control rules, what would stop it giving itself that ability too? Perhaps the server just takes it away immediately, or the Shexes limit what the client can do. 

Headers? Perhaps here is a minor difference that needs to be hardcoded.
(Such headers could also go into a .meta file)
   
ACLs are RdfSources and may be restricted by shapes.

This would not account for 
 * ?

### File Naming Convention

Because we want to determine if a URL refers by at most one request to the file system, we need to build up our agents from file system attributes that don't require searching the file system or directory. This works nicely for LDPC and LDPRs: 
 * LDP Containers: these are encoded as directories
 * LDP Resources: these are encoded as sybolic links pointing to the principal representation (e.g. `card.ttl`) or the latest version (e.g. `card.0.ttl`) ?

Can we extend this to server managed resources?
A reasonable idea is to have server managed resources such as acls be encoded as symboliclinks with a `.` in them, the dot distinguishing them from LDPRs. 
They have to be symbolic links for the same reason that normal LDPRs have: so that they can point to the default representation or the latest version.  

An intuition is to think of `.` s as delimiting a certain form of container. Indeed syntactically a `.`  separates namespaces the way the `/` does. 
Nevertheless, on all file systems I know of, there is a semantic difference: the `/` maps to a directory which groups other files, making it easy to step through the names and attributes. It would be feasible to implement `.` as directories too, to help keep server managed resources together, though such dirs would need a special convention to distinguish them from dirs for Basic Containers: perhaps a symlink `card -> card.r/`. We don't implement this yet. 

So symbolic links with a `.` identify server managed resources. 
But we notice that there are two quite different server managed resources:
 1. the different versions and representations of a resource
 2. The ACL for a resource - and perhaps other metadata for a resource, including acls on acls?
 
How different these and other things are still needs to be worked out, but it is clear that there are quite strong differences, and quite a lot of them. So we have all the extensions for mime types (over 100), the languages, the version extensions, the metadata extensions... 

So when a request comes in for a resource containing one or more dots, it is not clear from the existence of a dot what type of actor should take care of it. 

 * `.acl` as an acl for a Container, should be an acl actor.
 * `card.acl` is also an acl actor, but only the `card` actor can really tell the difference between `card.ttl` and `card.acl`, ... 
 
So the algorithm has to be that a container, receiving a message whose target name 
 1. does not contain a dot and 
    1. maps to a directory is routed to the child directory
    2. maps to a link is routed to the LDPR actor for the link
 2. starts with a dot, it is routed to a special actor for that resource managed by the container
 3. does contain a dot other than in the first position, then
    1. will be routed to a file based resource, whose name is the first part up to the dot
    2. that resource may route it on to another actor specialised for that extension.

So when arriving at the last stage of routing - when `name` can only refer to the next container or a resource in this container - then we have something like the following

```scala
val parts = name.split('.')
if parts(0) == "" then 
   if parts.size == 0 then ??? //the target of the resource is this container itself
   else ???	//we have special resource for the container
else 
   //send message on to parts(0) child resource
   //on receiving the message, that actor will continue as if the name started 
   // on from there with `parts(1)`
   //if parts(1) == "acl" then that would be equivalent to the directory 
   // receiving the ".acl" request.
  
```

and looking at the attributes on the file system. If it points to a directory and there is a dot then this returns an error.
If it points to a symbolic link, then pass the message on to the child resource actor. 


### Scripts interleaving Cmds from Server and Client                              

Could one create server managed resources using scripts, built on Plain HTTP? 
For example, would it be possible to enhance a client POST-command, into a script so that once the resource created, its ACL resource can then be created by the server? 

Note: this would require us to have scripts that can thread commands from the agent with commands from the client. 
But currently the Commands don't contain information about who is executing it. 
That information is in the `CmdMessage` object that wraps the command and so it covers the whole script.

Possible advantages:
* it could allow one to make new types of containers with their own properties just by developing such extra scripts showing what needs to happen on creation, deletion, ...
* could it help with content negotiation features?
* ... ?

Could this work by having two file-system name-spaces: one for user managed and one for server managed resources? We do seem to be pointing that way with anything involving a `.` being server managed.  

This looks like something worth exploring once the basic commands have been tested out...
