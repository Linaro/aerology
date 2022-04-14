# Debugging Prep

Aerology's debugging subcommands operate primarily on an application snapshot,
called a core, and provides a means of dumping a core.

At the moment, Taking a core is a two step process. If this is found
to be too comberson, it may be changed.

Aerology builds a single-file application package, called a Zephyr App
Package or ZAP for short, from a Zephyr build directory.
Aerology takes snapshots using this ZAP.
Some subcommands, such as `segments`, can work with both a zap and a core.

## Packing a ZAP

Aerology builds a ZAP with the `pack` subcommand.
This subcommand requires a build directory and also accepts an optional zap file
name.
The usage is:
```
aerology pack <BUILD_DIRECTORY> [OUT_FILE]
```

For example, I could package my version of the `net/dhcpv4_client` sample built
for the `lpcxpresso55s69_ns` platform with the following command:
``` bash
# Verify that this is indeed a zephyr build directory
$ ls build/lpcxpresso55s69_ns/net/dhcpv4_client/
app                  compile_commands.json  tfm_s_signed.hex
build.ninja          Kconfig                zephyr
cmake_install.cmake  modules                zephyr_modules.txt
CMakeCache.txt       tfm                    zephyr_ns_signed.hex
CMakeFiles           tfm_merged.hex         zephyr_settings.txt
# Package the build directory
$ aerology pack build/lpcxpresso55s69_ns/net/dhcpv4_client/
packed into "dhcpv4_client.zap"
```

As may be evedent from the above shell session, when no output file is specified,
Aerology infers a name from the last component of the build directory.

We can test that this is a working zap by running the `segments` subcommand.
Don't worry about what the output looks like for now, as we'll cover that in
another chapter.
What we're looking for is a lack of errors:
```bash
$ aerology segments -s dhcpv4_client.zap
Note: Not to scale.
Key: r = readable, w = writable, x = executable, z = zeroed on startup
     ┃ = overlapping section
           zephyr      tfm_s        bl2
300274e0┌──────────┐
        │       rwz│
300222a0└──────────┘
3002229c┌──────────┐
        │        rw│
30022000└──────────┘
3000ce64            ┌──────────┐
                    │       rwz│
3000b5fc            ┢━━━━━━━━━━┪
                    ┃        rw┃
3000b500            ┡━━━━━━━━━━┩
30007d60            │          │┌──────────┐
                    │          ││       rwz│
30005560            │          │└──────────┘
30005558            │          │┌──────────┐
                    │          ││        rw│
30002640            ┢━━━━━━━━━━┪│          │
                    ┃        rw┃│          │
300025c0            ┡━━━━━━━━━━┩│          │
30000840            ┢━━━━━━━━━━┪│          │
                    ┃        rw┃│          │
30000820            ┡━━━━━━━━━━┩│          │
30000400            │          │├──────────┤
                    │          ││       rwz│
30000000            └──────────┘└──────────┘
10044024┌──────────┐
        │       rwx│
10030000└──────────┘
1001df20            ┌──────────┐
                    │        rx│
100113e0            └──────────┘
1001133c            ┌──────────┐
                    │       rwx│
100060fc            │          │┌──────────┐
                    │          ││       rwx│
10000000            └──────────┘└──────────┘
```

No errors, so it looks like our ZAP is fine.

## Dumping a Core

Now that we have a working ZAP, we can use it to dump a core.
Aerology exposes the `dump` subcommand to dump a core using the information
in a ZAP.

The usage f the dump command is:
```bash
aerology dump <ZAP_FILE> [CORE_FILE]
```

Similar to the `pack` subcommand, the output file argument is optional, and
it will infer a name based on the ZAP passed as the first argument.
Also similar to `pack`, there is very little output.

```bash
$ aerology dump dhcpv4_client.zap
Wrote core dump to "dhcpv4_client.core.0"
```

The core dump that Aerology wrote is self-contained, and can be transfered
across machines with different archetectures and operating systems without
losing any context or having an different behavior.