## SoLid implementation in akka

Initial [SoLiD](https://github.com/solid/solid-spec) implementation with [akka](http://akka.io/).


### Run

After downloading this repository:

```
$ sbt
> compile
> run .
```

Then on the command line you can try downloading a resource with

```
$ curl -i -k --cert test/test-localhost.pem:test https://localhost:8443/src/main/scala/co/os/Solid.scala
```

