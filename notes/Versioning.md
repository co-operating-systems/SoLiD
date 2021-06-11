# Variants and Versioning
 
These may be very different topics, which should be kept apart.
          
## Versioning Standards 

* [RFC 7089: Memento](https://tools.ietf.org/html/rfc7089).
* See work on a [Memento Ontology](http://timetravel.mementoweb.org/guide/api/) see
  * [2017 discussion](https://groups.google.com/g/memento-dev/c/xNHUXDxPxaQ/m/DHG26vzpBAAJ) and
  * [github issue 245](https://github.com/fcrepo/fcrepo-specification/issues/245#issuecomment-338482569).
   
How much of memento can we implement quickly to get going, in order to test some other aspects such as access control? [RFC 5829 Link Relation Types for Simple Version Navigation between Web Resources](https://tools.ietf.org/html/rfc5829) looks like a good starting point.

## First attempt

Currently the versioning is based on a simple numbering scheme: Each version of a resource gets a number, and a symbolic link points to the last version.

We start by having one root variant for a resource, which can give rise to other variants built from it automatically (e.g. Turtle variant can give rise to RDF/XML, JSON/LD, etc...), but we keep the history of the principal variant. For now, we assume that we keep the same mime types throughout the life of the resource.
  
As things get more complex, it feels like all variants of a resource should end up in a container like structure, as it would allow one to get all the resources together and reduce the search space for the variants.

### LDP Resources	 

1. Container receives a POST for a new resource
   * `<card> -> <card>` create loop symlink (or we would need to point `<card>` to something that does not exist, or that is only partially uploaded, ...)
   * create file `<card.0.ttl>` and pour in the content from the POST or PUT
   * when finished relink `<card> -> <card.0.tll>`
2. Updating with PUT/PATCH follows a similar procedure
   * `card -> card.v0.ttl`  starting point
   * create `<card.1.ttl>` and save new data to that file
   * when finished relink `<card> -> <card.1.ttl>`
   * `<card.1.ttl>` can have a `Link: ` header pointing to previous version `<card.0.ttl>`.
    
### Server Managed Resources

Our convention for Server managed resources is that they consist of the root for a resource plus a specific extension. So the ACL for a container `/container/` will be `/container/.acl`. 
The ACL for a resource such as a personal profile document `card` will be `card.acl`

We think of ACLs as having a default content that links them to the parent ACL. 
There is thus no need to have a representation on the file system for these defaults. 
The resource has an "acl" link relation to a resource whose actor returns the default triple, but there need be no file info on the FS.
When an initial PUT is made, then the `card.acl` is created as a symbolic link pointing to `card.acl.0.ttl`

So ACLs only accept PUT and GET as methods (PATCH later). 
1. `card.acl` receives a PUT then the resource `card.acl.0.ttl` is created after the graph content of the PUT is parsed and verified, and `card.acl` is linked to `card.acl.0.ttl`.
2. If another edit is made then the new version is placed in `card.acl.1.ttl` the symbolic link is changed to `<card.acl> -> <card.acl.1.ttl>`.

### Variants

Later we may want to allow upload of say humanly translated variants of some content. At that point as the of variants becomes more complicated it looks like it may not be a bad idea to have a resource be a directory of its variants.

## Other methods to get at variants

Using file extension conventions as suggested allows for wide implementations on most file systems. The onlhy requirement there is symbolic links.


* one can place metadata about file (what mime types they encode, where the variants are, ...) in file attributes (Java supports that)
* one can place metadata into a conventionally named RDF file with info of how the different versions are connected)
* Place variants in a directory to speed up search for variants
  - specialised versioning File Systems
  - implement this over CVS or git, ...


Yet, it is not ideal.

## Coherence problem

When a resource is versioned, would one not want the links to in the versioned version to point to the precise versions that they were pointing to, rather than link to the latest versions? 

Advantages
* This would help give a real coherent historical overview of the data, helping one to understand what was really pointed to at a given time, and so understand the state of a conversation
* ... 

But,  
* does it require relinking all documents before they get archived? 
  In the current implementations, this would require finding for all links in a given document all the resource versions those links are pointing to, and rewriting that document to point to those versions! 
* how would it works for links to resources on the open web?
            
### Versioning the whole Server

At least for all local data, this indicates that a system versioning the whole web server would at least be locally more correct. I am thinking of a setup where a change to  `/people/henry/card` places the old version - and all other resources in the container `/2021/06/01/10/` so that the above `card` resource can be found at `/2021/06/01/people/henry/card` with all its inter-relations there.
Note: that would work well if all resource were exclusively written with relative URLs, when linking to local resources, as those files would form the same tree in the archive directory.


This may work quite well actually on a versioned file system. So it would
be worth finding out which filesystems are best suited for this. On such a filesystem making a change to a resource would involve:
 1. making a snapshot of the FS and naming it, 
 2. creating a read-only archive of snapshots.

### Access Control on Archives

Those archive dirs should be access controlled, so as not to "pollute" the open web with too many versioned links, in a different way perhaps to the latest version. 
This may indicate that archived versions have both the archived ACRs, but also effective ones that can override the archived ones.
