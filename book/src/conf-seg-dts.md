# Config, Segments, Device Tree

Aerology stores the Zephyr Config and Device Tree in both ZAPs and cores.
These files are accessable with a few subcommands.

## Config, the `config` subcommand

The `config` subcommand dumps the Kconfig that was used for the build of
Zephyr in the ZAP or core.

For example:
```bash
$ aerology config dhcpv4_client.zap
--- Lots of config output ---
#
# Boot Options
#
# CONFIG_IS_BOOTLOADER is not set
# CONFIG_BOOTLOADER_MCUBOOT is not set
# CONFIG_BOOTLOADER_BOSSA is not set
# end of Boot Options

#
# Compatibility
#
CONFIG_COMPAT_INCLUDES=y
# end of Compatibility
```

Further, the config may be queried from a core dump as well.
If the core dump and the ZAP have been built from the same build of
Zephyr, their configs will be the same.

```bash
$ diff <(aerology config dhcpv4_client.zap) <(aerology config dhcpv4_client.core.0)
--- No Output ---
```

## Device Tree, the `dts` subcommand

Similar to the `config` subcommand, the `dts` subcommand dumps the device
tree source used in the build.

For example:
```
$ aerology dts dhcpv4_client.core.0
--- Lots of DTS output ---
        reserved-memory {
                #address-cells = < 0x1 >;
                #size-cells = < 0x1 >;
                ranges;
                code: memory@01060000 {
                        reg = < 0x1060000 0x60000 >;
                };
                ram: memory@21000000 {
                        reg = < 0x21000000 0x200000 >;
                };
        };
};
```

As with the `config` subcommand, the `dts` subcommand works on both ZAPs
and cores.

## Memory Layout, the `segments` subcommand

The memory layout of a Zephyr (and TFM) application can be visually inspected
with the `segments` subcommand:

```bash
$ aerology segments --summary dhcpv4_client.core.0
Note: Not to scale.
Key: r = readable, w = writable, x = executable, z = zeroed on startup
     ┃ = overlapping section
           zephyr      tfm_s        bl2
31007928┌──────────┐
        │       rwz│
31000418└──────────┘
31000414┌──────────┐
        │        rw│
31000000└──────────┘
3000ff70            ┌──────────┐
                    │       rwz│
3000bf38            ┢━━━━━━━━━━┪
                    ┃        rw┃
3000bbc0            ┡━━━━━━━━━━┩
30005f40            │          │┌──────────┐
                    │          ││        rw│
30003ac0            ┢━━━━━━━━━━┪│          │
                    ┃        rw┃│          │
30003a80            ┡━━━━━━━━━━┩│          │
30002020            ┢━━━━━━━━━━┪│          │
                    ┃        rw┃│          │
30002000            ┡━━━━━━━━━━┩│          │
30000400            │          │├──────────┤
                    │          ││       rwz│
30000000            └──────────┘└──────────┘
11076384┌──────────┐
        │       rwx│
11060000└──────────┘
1105f500            ┌──────────┐
                    │        rx│
1105f4c0            └──────────┘
11021280            ┌──────────┐
                    │        rx│
11009fe0            └──────────┘
11009f68            ┌──────────┐
                    │       rwx│
11000000            └──────────┘
100055e0                        ┌──────────┐
                                │       rwx│
10000000                        └──────────┘
```

The above shows the `--summary` output, because the unsummarized output is
quite long.

Without summary it looks like:
```
$ aerology segments dhcpv4_client.core.0
Note: Not to scale.
Key: r = readable, w = writable, x = executable, z = zeroed on startup
     ┃ = overlapping section
                     zephyr                          tfm_s                            bl2
31007928┌──────────────────────────────┐                                   
        │noinit                     rwz│                                   
31002020├──────────────────────────────┤                                   
        │bss                        rwz│                                   
31000418└──────────────────────────────┘                                   
31000414┌──────────────────────────────┐                                   
        │net_l2_area                  r│                                   
31000404├──────────────────────────────┤                                   
        │net_if_dev_area             rw│                                   
310003e8├──────────────────────────────┤                                   
        │net_if_area                 rw│                                   
310003a8└──────────────────────────────┘                                   
310003a4┌──────────────────────────────┐                                   
        │_net_buf_pool_area          rw│                                   
3100034c├──────────────────────────────┤                                   
        │k_sem_area                  rw│                                   
310002ec├──────────────────────────────┤                                   
        │k_msgq_area                 rw│                                   
310002bc├──────────────────────────────┤                                   
        │k_mutex_area                rw│                                   
3100026c├──────────────────────────────┤                                   
        │k_mem_slab_area             rw│                                   
31000234├──────────────────────────────┤                                   
        │log_dynamic_sections        rw│                                   
310001d0├──────────────────────────────┤                                   
        │device_states               rw│                                   
31000188└──────────────────────────────┘                                   
31000187┌──────────────────────────────┐                                   
        │datas                       rw│                                   
31000000└──────────────────────────────┘                                   
3000ff70                                ┌──────────────────────────────┐   
                                        │.TFM_BSS                   rwz│   
3000bf40                                └──────────────────────────────┘   
3000bf38                                ┌──────────────────────────────┐   
                                        │.TFM_DATA                   rw│   
3000bbc0                                ├──────────────────────────────┤   
                                        │.TFM_PSA_ROT_LINKER_BSS    rwz│   
30005f40                                │                              │┌──────────────────────────────┐
                                        │                              ││.heap                      rwz│
30004f40                                │                              │├──────────────────────────────┤
                                        │                              ││.msp_stack                 rwz│
30003ac0                                ├──────────────────────────────┤│                              │
                                        │.TFM_PSA_ROT_LINKER_DATA    rw││                              │
30003a80                                ├──────────────────────────────┤│                              │
                                        │.TFM_APP_ROT_LINKER_BSS    rwz││                              │
30003740                                │                              │└──────────────────────────────┘
30003728                                │                              │┌──────────────────────────────┐
                                        │                              ││.bss                       rwz│
30002020                                ├──────────────────────────────┤│                              │
                                        │.TFM_APP_ROT_LINKER_DATA    rw││                              │
30002000                                ├──────────────────────────────┤│                              │
                                        │.heap                      rwz││                              │
30001000                                ├──────────────────────────────┤│                              │
                                        │.psp_stack                 rwz││                              │
30000800                                ├──────────────────────────────┤│                              │
                                        │.msp_stack                 rwz││                              │
30000494                                │                              │├──────────────────────────────┤
                                        │                              ││.data                       rw│
30000400                                ├──────────────────────────────┤├──────────────────────────────┤
                                        │.tfm_bl2_shared_data       rwz││.tfm_bl2_shared_data       rwz│
30000000                                └──────────────────────────────┘└──────────────────────────────┘
11076384┌──────────────────────────────┐                                   
        │rodata                       r│                                   
11071bfc├──────────────────────────────┤                                   
        │device_handles               r│                                   
11071b90├──────────────────────────────┤                                   
        │shell_root_cmds_sections     r│                                   
11071b38├──────────────────────────────┤                                   
        │shell_area                   r│                                   
11071b08├──────────────────────────────┤                                   
        │log_backends_sections        r│                                   
11071af8├──────────────────────────────┤                                   
        │log_const_sections           r│                                   
11071a30├──────────────────────────────┤                                   
        │sw_isr_table                rw│                                   
11071630├──────────────────────────────┤                                   
        │devices                      r│                                   
11071480├──────────────────────────────┤                                   
        │initlevel                    r│                                   
110713a0├──────────────────────────────┤                                   
        │.ARM.exidx                   r│                                   
11071398├──────────────────────────────┤                                   
        │text                        rx│                                   
11060640├──────────────────────────────┤                                   
        │rom_start                  rwx│                                   
11060000└──────────────────────────────┘                                   
1105f500                                ┌──────────────────────────────┐   
                                        │.gnu.sgstubs                rx│   
1105f4c0                                └──────────────────────────────┘   
11021280                                ┌──────────────────────────────┐   
                                        │.ARM.exidx                   r│   
11021278                                ├──────────────────────────────┤   
                                        │.psa_interface_thread_call  rx│   
11021180                                ├──────────────────────────────┤   
                                        │.TFM_UNPRIV_CODE            rx│   
11009fe0                                └──────────────────────────────┘   
11009f68                                ┌──────────────────────────────┐   
                                        │.ER_TFM_CODE                rx│   
110056c0                                ├──────────────────────────────┤   
                                        │.TFM_APP_ROT_LINKER         rx│   
11004940                                ├──────────────────────────────┤   
                                        │.TFM_PSA_ROT_LINKER         rx│   
11000f40                                └──────────────────────────────┘   
11000f24                                ┌──────────────────────────────┐   
                                        │.TFM_SP_LOAD_LIST            r│   
11000d34                                ├──────────────────────────────┤   
                                        │.zero.table                 rw│   
11000d1c                                ├──────────────────────────────┤   
                                        │.copy.table                 rw│   
11000cf8                                ├──────────────────────────────┤   
                                        │.TFM_VECTORS                rx│   
11000400                                └──────────────────────────────┘   
100055e0                                                                ┌──────────────────────────────┐
                                                                        │.zero.table                 rw│
100055d0                                                                ├──────────────────────────────┤
                                                                        │.copy.table                 rw│
100055b8                                                                ├──────────────────────────────┤
                                                                        │.ARM.exidx                   r│
100055b0                                                                ├──────────────────────────────┤
                                                                        │.text                       rx│
10000000                                                                └──────────────────────────────┘
```