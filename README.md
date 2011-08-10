CoreMac packages
===============

The "CoreMac" packages provide an interface to Apple's APIs for programming on 
Mac OS X and iOS.

Most GUI programming for the Mac and iOS is written in the Objective-C programming
language.  However, Apple also provides a C interface for many of its APIs.
These packages provide a simpler interface than bindings like HOC, since they
do not need to model Objective-C's object-oriented interface.

Several data types are \"toll-free bridged\" between Foundation and 
Core Foundation, meaning that the corresponding C and Objective-C 
types may be used interchangeably.  
The documentation of a module will note if 
an object is toll-free bridged.  

See the "examples" directory for suggestions on how to use this package.

Installation
-------------
For building 64-bit applications:
    
    cabal update
    cd CoreFoundation
    cabal install
    cd ../CoreGraphics
    cabal install

For building 32-bit applications:

    cabal update
    cd CoreFoundation
    cabal install -f32bit
    cd ../CoreGraphics
    cabal install -f32bit

