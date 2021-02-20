## SoLiD implementation in akka

Initial [SoLiD](https://github.com/solid/solid-spec) implementation with [akka](http://akka.io/).

Reactive Solid is developed as part of the [Solid-Control](https://nlnet.nl/project/SolidControl/) project, which was funded by the the [Next Generation Internet (NGI0)](https://nlnet.nl/NGI0/) in their [Privacy and Trust Enhancing Technologies (PET)](https://nlnet.nl/PET/) call.

### Run

After downloading this repository you can start the server as follows:

```
$ sbt
> compile
> run .
```

Then on the command line you can try downloading a resource with

```
$ curl -i http://localhost:8080/README.md
```

