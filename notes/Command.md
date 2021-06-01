## Interaction of Graph and Streaming views

We start from having built a streaming LDP-enabled Web Server without access control.
The server can create LDP resources and LDP Containers with POST, change content with PUT, as well as DELETE content too. 
We also have implemented a little bit of versioning.

We next want to add access control, which requires the Guard on the server to query a set of linked RDF graphs: see illustration in issue 210 [add :imports relation](https://github.com/solid/authorization-panel/issues/210).

To make the code easier to write, we need a layer that can work with graphs or DataSets as objects.
Parts of the datasets will be local, others remote, but the Guard should be written in such a way that those facts don't show up in the code.
I.e. we don't want any `if local ... else if remote ...` type code littered everywhere in our scripts.

Using Free Monads, we can build Scripts that fetch data at a URL, transform it, and use that data to fetch something else. The data returned can be DataSets as Cofree Comonads.

## HTTP Requests vs Scripts 

```scala
type Script[A] = cats.free.Free[LDPCmd,A]
```

Scripts are sent to actors as `LDPCmd[Script[T]]`. 
On receiving a Script, an actor executes one layer before sending on the result transformed by mapping the contintuation function provided in the command, and flattening the result.

Each layer, just like an HTTP Request, has a target URL. 

```scala
sealed trait LDPCmd[A]:
   def url: Uri
```

### Are HTTP Requests just one-layer Scripts? Why not?

It would be helpful to have a coherent view of the relation between them. 
Even better would be to find a unified view.

Note that on the error side there is a strong relation too, and for good reasons.

1. We want our Scripts to keep track of the HTTP interactions, redirects, errors, authentication requests... so that we can find bugs, let clients know of problems, retry later, or indeed know when a graph's validity expires
```scala 
def get(key: Uri): Script[Response]
case class Response(meta: Meta, content: Try[Rdf#Graph])
```  
2. When a Script gets routed to an actor, it can fail because the actor does not exist: such an error must be forwarded to the Script monad, which can then act on that. 
This is similar to a Routed HTTPRequest that fails.
3. When the Script reaches an actor, it can fail for all the reasons that HTTP Requests fail, since it relies on HTTP Requests to execute! But it can also fail marshalling the stream to form the graph, and other actions of the Command, such as querying, etc...

Assume we have not one a  `get(url)` script but also a `fetch(url)` one based directly on HttpRequests

```scala
case class Get[A](url: Uri, k: Response => A) extends LDPCmd[A]
case class HTTP[A](req: HttpRequest, k: HttpResponse => A) extends LDPCmd[A]

def get(key: Uri): Script[Response] = ???
def fetch(req: HttpRequest): Script[HttpResponse] = ???
``` 

Then a `get` could just be defined as a `fetch` composed with a function to turn an HttpResponse into a Graph. Something like

```scala 
def get(url: Uri) = fetch(url).flatMap(_.parseResponseToRDFGraph)
``` 

Turning `get` into an independent command requires that the actor receiving the command have the ability to deserialise the stream (`parseResponseToRDFGraph`). 
It also allows that actor to return a cached copy, avoiding serialisation and deserialisation steps if possible.

So the above seems to suggest that

1. we can build HTTP Requests into the Script layer, giving us a unified script based view,
2. Other LDP based commands such as `Get` (which should perhaps be called `GetRDF`) are commands that can be built out of the basic HTTP ones. Something like this:
```scala
  def GetRDF(uri: Uri) = GetCache(uri).orElse{
    for { 
       stream <- HTTPGet(uri)
       graph <- CachePut(uri, stream.parseToRDFGraph)
    } yield graph
  }
```

This snippet actually suggests that if one has a `Cache` command, one could program the `GetRDF` directly from HTTP and cache, passing the deserialisation of the stream as part of the monad.

This raises the question if one can find the atomic commands that are needed in addition to HTTP.

## How should the Scripts be interpreted?

Should they be sent to a set of DataBase Actors that keep a cache of the Data, fetching the data using `fetch` above, or should the Scripts be sent to individual actors?

### 1) A DB Actor
 
   We could have a DB Actor that would receive such a command and execute it by looking up the data in the DB. 
If the data were missing it could just GET it with a streaming HTTP Request and this would work wherever the data turned out to be. 

Advantages:
  - clean separation between the streaming data-on-the-filesystem view and the Graph view
  - the management of the memory of the DB would be located with the DB Actor, which could remove graphs when no longer requested enough... 
  - It fits a certain idea of a graph store, and in a way this is a reproduction of such a store

Disadvantages:
  - The graph is distant from the data-on-the-filesystem, and so this would require synchronization of the DB with the Web Server
   (That could be as simple as the actor sending a notification message of a change to the DB Actor, which we also want for remote web servers) 
  - 

### 2) Send Scripts to actors

The Free Monads Scripts can be sent to the actors to be executed there as in RWW play.

Advantages:   
  - As the Actor is in control of the state of the resource it will know to have a fresh, up-to-date version of the graph. 
  - Good reasons for having the actor have an interpreted view of the data:
    * Creation of new resources with POST should check the well-formedness of the content
    * PUT, especially on RDF content: one should check that the data is Well Formed. Especially true of ACLs.
    * PATCH also needs access to the interpreted data
  - After fetching the first ACL Dataset, new requests arriving on an actor don't need any new message passing to check the ACL rules

Disadvantages:
  - The interaction of Cmd View and HttpRequest view is more complicated
    (But perhaps the two levels can be insulated or brought together cleanly.)

The DB view idea has the advantage of making the separation clear and showing how
they can interact. 
So we need to find a way to have this clean separation in the actors as individual DBs view.

## Everything as a Script?

What if one built everything on `HttpRequest` as the basic script building blocks?  Perhaps one would just need a few extra important Commands such as we saw above to enable a caching mechanism? 

What could one do that way?

### No need for an access Control layer?

Could access control itself be turned into a script?
On receiving an HTTP Request the server would turn the request into a script prepended by an access control script. 
After all: access control can be reduced to fetching and traversing graphs. 
The main idea here would be that we make **no** distinction between scripts interacting with remote resources and those interacting with local ones! Can it be done? 
Let us try this out.

1. the server receives an HTTP Request for `/blog/post1` 
2. That request is sent to the actor, but with an extra layer on access control based failure:
   1. find the acl link in the Header of the failed response for `/blog/post1` 
   2. Make a request for it as a graph (a `GetGraph` command) so that one can avoid serialisation if the object is cached,
   3. Follow that and the `:includes` relations as per issue 210 returning a CoFree Comonad DataSet as shown by the `LDPCmd.fetchWithImports` function (which one will want to cache again)
   4. verify that the authenticated agent is authorized. Note: this requires the command is able to access the ID or properties of the agent making the request!
   5. if the agent verifies, then make the request again
   
Note, the above does not work identically with local and remote resources.
That is because if access control is not done by another agent, built into the resource, then step 1 will always go through on local resources. One could try to replace that with a step in 1. that only does a HEAD request, but that would slow down interactions with remote resources, as each request for them would need a HEAD first. Also the client making the request should not be trying to restrict what it can access remotely (though that may be useful in some cases) but should rather be working out what credentials it may need for remote resources.
   
Even though it would not make sense to add access control to a request by changing the request to one with an access control stack, it does make sense to have the Guard express its logic in terms of a Free Monad on HttpRequests.
   

