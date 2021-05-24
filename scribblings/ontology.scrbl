#lang scribble/manual

@(require "shared.rkt")

@title{@black{The Common Ontology}}

For participants to connect to the server, they need to understand the interaction protocol, which
specify the sequence of remote calls and their signatures.  While each message is just a piece JSON
data, the signatures interpret these pieces of data in the context of the game.

Since even a detailed interpretation may leave questions about their meaning with respect to the
rules of the game. the repository's `Common` directory publishes the code that interprets the remote
calls with respect to the server and the sample player in this repository.

