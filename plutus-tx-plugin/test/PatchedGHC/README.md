# Issues with the Patched GHC

There are 5 cases that break **plutus compilation** via the patched ghc:

## StrictData

When an ADT has at least one strict field.

A workaround is to place the `data` declaration in a separate module from the module that calls `Plutus.compile/plc`

## InstanceSameMod

Instances (no matter the number of methods) which are defined *and* compiled by plutus *inside the same module*.

Note: you cannot rely on plutus `compile` TemplateHaskell-helper, instead you have to use the more verbose `plc` marker,
because of TemplateHaskell's stage restriction.

A workaround is to place all the instance definitions in separate modules from the module that calls `Plutus.compile/plc`.
These instances can be next to the `class` declararation or orphans on different modules.

## InstanceContext

Instance definitions that contain some context, e.g. `instance Context => Class a b where`
are broken when compiling to plutus. The size and contents of the Context do not matter for this breakage.

There is no workaround currently.

## OmitInterface

An example with separate modules Lib (for typeclass declaration and instances, no orphans) and Use (for plutus compilation/use site).

This breaks plutus compilation for some methods which belong to typeclasses of >=2 number of methods.
The error is that the wrong method implementation is returned upon querying the extensible interface.
For example, in a typeclass with 2 methods `method1` and `method2, querying for the RHS of `method2` returns
the implementation RHS of `method1` which most likely will run into a type error.

Workaround: Use `-fomit-interface-pragmas` or use  `ghc -O`, aka `ghc -O1' (`cabal` defaults to `-O`)

## plutus-ledger:Ledger.Typed.Scripts.Validators.forwardingMPS

This yields the same method mismatch error as the `OmitInterface` above. The workaround of `OmitInterface`
does not seem to help. It has to do with the 2-typeclass `Data` and the method `fromData`.
