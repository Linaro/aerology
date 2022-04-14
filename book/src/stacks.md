# Stacks

Aerology currently supports 2 subcommands that help with looking through
stacks: `stacks` for a summary of the filled-level of all thread stacks
Aerology can find, and `backtrace` for backtracing all stacks in the system.

## `stacks` subcommand

The stacks subcommand renders a pretty version of stack usage in a table format:

```bash
$ aerology stacks dhcpv4_client.core.0
Key: █: currently in use ▒: used in the past ░: never used
name                 used    max   size
zephyr::logging       32b    32b   768b ░░░
zephyr::shell_uart    32b    32b  2048b ░░░░░░░░
zephyr::idle 00       32b    32b   320b ░
zephyr::main          88b   216b  1024b ░░░░
tfm_s::3000bf40      312b  2440b  8192b █▒▒▒▒▒▒▒▒▒░░░░░░░░░░░░░░░░░░░░░░░░░
tfm_s::3000bf88      192b   192b  2688b ░░░░░░░░░░░
tfm_s::3000bfd0      168b   776b  2048b ▒▒▒░░░░░
tfm_s::3000c018      176b   644b  1664b ▒▒░░░░░
tfm_s::3000c060      120b   256b  1280b ▒░░░░
tfm_s::3000c0a8       72b   300b  1024b ▒░░░
tfm_s::3000c0f0       72b    20b   256b ░
```

This might be helpful for determining the maximum stack usage of a given
thread, for example.

## `backtrace` subcommand

The backtrace subcommand is where Aerology really shines; it backtraces all
threads in the system including stacks of TFM partitions:

```bash 
$ aerology backtrace dhcpv4_client.core.0
Registers
  ├─ 11008a60 in tfm_access_violation_handler
  ╞═ Exception Handler Called
  ├─ 01069bfc in smsc_init
  ├─ 01069ce3 in eth_init
  ├─ 01069fe5 in z_sys_init_run_level
  ├─ 0106a1af in bg_thread_main
  ├─ 0106bae1 in z_thread_entry
  └─ 010645e3 in arch_switch_to_main_thread
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
Thread tfm_s::sp_crypto
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::sp_initial_attestation
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::sp_ps
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::sp_its
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::sp_platform
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
Thread tfm_s::sp_ns_agent
  └─ 11008c85 in tfm_nspm_thread_entry
Thread tfm_s::idle
  └─ 11008ca5 in tfm_idle_thread
```

Optionally, you can have Aerology dump the registers at each stack frame with
the `--regs`.
The following in an excert from the same backtrace with `-r` applied:
```
Thread zephyr::main
  ├─ 010644f8 in arch_swap
  │  R0  00000000 R1  01071470 R2  01071618 R3  01069ce3
  │  R4  00000000 R5  00000000 R6  01071480 R7  0106a1a1
  │  R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
  │  R12 00000000 Sp  210037c8 Lr  0106b8dd Pc  010644f8
  │  Psr 61000000
  ├─ 0106b8dd in k_sys_work_q_init
  │  R0  00000000 R1  01071470 R2  01071618 R3  01069ce3
  │  R4  01071470 R5  00000000 R6  01071480 R7  0106a1a1
  │  R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
  │  R12 00000000 Sp  210037e0 Lr  01069fe5 Pc  0106b8dd
  │  Psr 61000000
  ├─ 01069fe5 in z_sys_init_run_level
  │  R0  00000000 R1  01071470 R2  01071618 R3  01069ce3
  │  R4  0106a1a1 R5  21000e98 R6  21003800 R7  0106a1a1
  │  R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
  │  R12 00000000 Sp  210037f0 Lr  0106a1af Pc  01069fe5
  │  Psr 61000000
  ├─ 0106a1af in bg_thread_main
  │  R0  00000000 R1  01071470 R2  01071618 R3  00000000
  │  R4  0106a1a1 R5  21000e98 R6  21003800 R7  0106a1a1
  │  R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
  │  R12 00000000 Sp  210037f8 Lr  0106bae1 Pc  0106a1af
  │  Psr 61000000
  ├─ 0106bae1 in z_thread_entry
  │  R0  00000000 R1  01071470 R2  01071618 R3  00000000
  │  R4  0106a1a1 R5  21000e98 R6  21003800 R7  0106a1a1
  │  R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
  │  R12 00000000 Sp  21003800 Lr  010645e3 Pc  0106bae1
  │  Psr 61000000
  └─ 010645e3 in arch_switch_to_main_thread
     R0  00000000 R1  01071470 R2  01071618 R3  00000000
     R4  0106a1a1 R5  21000e98 R6  21003800 R7  0106a1a1
     R8  01064cd8 R9  01064cd8 R10 01064cd8 R11 01064cd8
     R12 00000000 Sp  21003800 Lr  010645e3 Pc  010645e3
     Psr 61000000
```

Since each stack frame takes up 6 lines instead of 1, all of the backtraces
together becomes quite long.