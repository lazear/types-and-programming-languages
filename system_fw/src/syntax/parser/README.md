# Parser

We use a handwritten recursive descent parser. In general, there is a top-level entry function for parsing of `types`, `expressions`, `declarations`, and `patterns`. Each of these functions attempts to match against the current token, without popping it. If a suitable match is found, then we dispatch to a function specifically for parsing that token, which may then pop off the current token. Once the current token has been popped, we actually begin to return real errors. Errors that occur before the current token has been popped may be generally ignored.
