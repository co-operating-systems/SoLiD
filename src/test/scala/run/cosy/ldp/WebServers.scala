package run.cosy.ldp

/** set of test web servers that can be called by test suites */
object WebServers {
	val imports         : TestCompiler = TestCompiler(ImportsTestServer)
	val connectedImports: TestCompiler = TestCompiler(ConnectedImportsTestServer)

}
