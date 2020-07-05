# HGLL

Haskell GLL (HGLL) is an effort towards a certifiable GLL parsing framework
written in Haskell.

**NOTE!** Right now HGLL does not have a general parser algorithm implemented,
so it is not a GLL implementation. Details on why can be found in my [master's
thesis](elsewhere) on the subject of certifiable, general parsing, in a purely
functional setup.

# Installation

* Clone this repository;
* Move into the src directory and compile `hgll.hs`. Something like this:

    ```sh
    ghc -outputdir ~/"cloned repository root path"/out -o "your prefered output path" hgll.hs
    ```
* You can add `"your prefered output path"` to your `$PATH`, or use `hgll`
locally.

**NOTE!** The compilation will produce a bunch of warnings, they should not
cause any issue.

# Usage

For now, hgll is quite limited one can only use it to generate default parsers.

```sh
hgll gen "an ISO EBNF input file"
```

**NOTE!** The generator does not pretty print its output. The user must format
it by hand.

# Misc

If you are "inspecting" this repository for any reason, please check the
additional, local, `README` files. For example, directory `t/qc/` has a local
`README` with instructions on how to setup some QuickCheck tests.
