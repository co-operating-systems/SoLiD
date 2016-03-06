## SoLiD implementation in akka

Initial [SoLiD](https://github.com/solid/solid-spec) implementation with [akka](http://akka.io/).

### Prerequisits

This version requires a version of akka that integrates the patch [in akka PR19787](https://github.com/akka/akka/pull/19787). Clone the git repo spray/akka, checkout [w/18351-implement-dynamic-client-certificates](https://github.com/spray/akka/commits/w/18351-implement-dynamic-client-certificates), and then run

```
$ sbt -Dakka.scaladoc.diagrams=false publishLocal
```

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

