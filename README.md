# SDP IO Extension

SDP utils for file input/output.

## Reasons

SDP.SafePrelude doesn't export functions from System.IO. Thus, SDP is I/O
neutral and you can use any other implementation. sdp-io is yet another way to
generalize I/O.

## Functionality

SDP IO is designed for simple I/O in the data representation that you need right
now. You can easily combine binary and text, strict and lazy I/O in the same
namespace (see sdp4bytestring and sdp4text). This extension is useful when you
are already use SDP, because otherwise you will have to use qualified or partial
imports.

* The IsFilePath class is generalized by location (file, network, abstract,
etc.) resource
* The IsFile class is generalized by representation I/O resource, binary or text
* The IsTextFile class is generalized by representation I/O text resource,
extends IsFile

## Versioning

sdp-io follows of the [Haskell PVP](https://pvp.haskell.org) and SDP extension
rules.


