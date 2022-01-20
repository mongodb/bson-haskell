# End of Life Notice
The MongoDB bson-haskell library is now officially end-of-life (EOL). No further development, bugfixes, enhancements, documentation changes or maintenance will be provided by this project and pull requests will no longer be accepted.

## About

Haskell library for the BSON encoding. Used in MongoDB.
BSON documents are JSON-like objects with a standard binary encoding.
A BSON Document is serialized to a standard binary encoding defined at <http://bsonspec.org>. This implements version 1 of that spec.

A BSON Document is an untyped (dynamically type-checked) record. I.e. it is a list of name-value pairs, where a Value is a single sum type with constructors for basic types (Bool, Int, Float, String, and Time), compound types (List, and (embedded) Document), and special types (Binary, Javascript, ObjectId, RegEx, and a few others).
