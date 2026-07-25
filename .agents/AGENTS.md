# Testing and Verification Rule

- Always run `sbt test` to ensure that all changes pass the tests.
- If you encounter environment issues running tests (such as macOS App Sandbox restrictions preventing Unix Domain Sockets for `sbt`), instruct the user to run the tests instead. Do not proceed to subsequent phases of work without verifying that tests pass.

# Documentation Synchronization Rule

- Whenever project naming, organization, build metadata, or repository identifiers change in configuration files (such as `build.sbt` or `pom.xml`), you must update `README.md` to reflect those changes.

