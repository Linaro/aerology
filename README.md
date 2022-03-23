# Aerology

Inspect Zephyr and TF-M applications, post mortem.

Aerology allows for analysis and inspection of symbols and stacks from a core
dump taken from a running zephyr application.

/!\ Warning /!\

This software is at a very early stage of development and the command line
interface will change without notice, possibly more than once a day.
Further, the documentation is sparse at best.

# What does it do?

Currently aerology allows a user to do the following with a core dump of
zephyr:
  * Pack build diectorys into "zap" zephyr app packages
  * print DTS and Config
  * Take core dumps over gdb remot protocol
  * disassemble functions
  * print symbol type and size info
  * visualize memory layout from segments & program headers
  * query symbols to print values
  * visualize stack usage
  * Take backtraces from all threads in zephyr & tfm
  
# How do I use it?

The current workflow looks something like:

1) Do a west build
2) Package the west build into a zap with `aerology pack <build-dir>`
3) Run your application in qemu (support for other gdb servers pending)
4) Take a core dump with `aerology dump <zap>`
5) Inspect the core dump with the remaining subcommands, e.g. `aerology backtrace <core>`

# Exmaple output

Pretty-Print all of the tfm partition control context structures:

```
    ;aerology symbol-value dhcpv4_client.core.0 'partition_listhead.llnodes(next).ctx_ctrl'
3000bf60: (struct context_ctrl_t) {
    .sp = (uint32_t) 805347216 /*0x30009f90*/,
    .sp_limit = (uint32_t) 805339336 /*0x300080c8*/,
    .reserved = (uint32_t) 805347288 /*0x30009fd8*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000bfa8: (struct context_ctrl_t) {
    .sp = (uint32_t) 805350664 /*0x3000ad08*/,
    .sp_limit = (uint32_t) 805348168 /*0x3000a348*/,
    .reserved = (uint32_t) 805350736 /*0x3000ad50*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000bff0: (struct context_ctrl_t) {
    .sp = (uint32_t) 805321152 /*0x300039c0*/,
    .sp_limit = (uint32_t) 805319272 /*0x30003268*/,
    .reserved = (uint32_t) 805321224 /*0x30003a08*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000c038: (struct context_ctrl_t) {
    .sp = (uint32_t) 805352976 /*0x3000b610*/,
    .sp_limit = (uint32_t) 805351488 /*0x3000b040*/,
    .reserved = (uint32_t) 805353048 /*0x3000b658*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000c080: (struct context_ctrl_t) {
    .sp = (uint32_t) 805354312 /*0x3000bb48*/,
    .sp_limit = (uint32_t) 805353152 /*0x3000b6c0*/,
    .reserved = (uint32_t) 805354384 /*0x3000bb90*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000c0c8: (struct context_ctrl_t) {
    .sp = (uint32_t) 805356952 /*0x3000c598*/,
    .sp_limit = (uint32_t) 805356000 /*0x3000c1e0*/,
    .reserved = (uint32_t) 0 /*0x0*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
3000c110: (struct context_ctrl_t) {
    .sp = (uint32_t) 805357208 /*0x3000c698*/,
    .sp_limit = (uint32_t) 805357024 /*0x3000c5e0*/,
    .reserved = (uint32_t) 0 /*0x0*/,
    .exc_ret = (uint32_t) 4294967293 /*0xfffffffd*/,
}
```

Backtrace all threads in Zephyr and TF-M

```
    ;aerology backtrace dhcpv4_client.core.0
Thread zephyr::idle 00
  ├─ 0106bad4 in z_thread_entry
  └─ aaaaaaaa in <unknown>
Thread zephyr::logging
  ├─ 0106bad4 in z_thread_entry
  └─ aaaaaaaa in <unknown>
Thread zephyr::main
  ├─ 010644f8 in arch_swap
  ├─ 0106b8dd in k_sys_work_q_init
  ├─ 01069fe5 in z_sys_init_run_level
  ├─ 0106a1af in bg_thread_main
  ├─ 0106bae1 in z_thread_entry
  └─ 010645e3 in arch_switch_to_main_thread
Thread zephyr::shell_uart
  ├─ 0106bad4 in z_thread_entry
  └─ aaaaaaaa in <unknown>
Thread tfm_s::3000bf40
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::3000bf88
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::3000bfd0
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::3000c018
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::3000c060
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::3000c0a8
  └─ 11008c85 in tfm_nspm_thread_entry
Thread tfm_s::3000c0f0
  └─ 11008ca5 in tfm_idle_thread
```

Show a summary of the segments described by the zap metadata:

```
    ;aerology segments -s dhcpv4_client.core.0
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