logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// The bblfish.net repository
resolvers += "bblfish.net repository" at "http://bblfish.net/work/repo/releases/"

resolvers += Resolver.url("bblfish ivy repository",url("http://bblfish.net/work/repo/ivy/releases/"))(Resolver.ivyStylePatterns)
