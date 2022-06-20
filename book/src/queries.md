# Queries

Aerology's `query` subcommand is easily it's most general purpse tool.
It provides a means to inspect arbitrary objects (e.g. structs, unions, etc.)
in a C-style fromat or a hexdump.


## Syntax

The syntax of a query is inspired by `jq`, and many of the operators are postfix.
An Aerology query is structured as a global, with optional postfix operators,
followed by zero or more `|` separated "filters".

### Global
Unlike `jq`, the initial value is provided by global optionally prefixed with a
executable namespace.
For example, to get the zephyr kernel struct, either of the following queries
produce the same result:
```
_kernel
```
```
zephyr::kernel
```

Executing a query with just the global prints the whole structure C-style:

```bash
$ aerology query dhcpv4_client.core.0 zephyr::_kernel
21001ec0: (struct z_kernel) {
    .cpus = {
        (struct _cpu) {
            .nested = (uint32_t) 0 /*0x0*/,
            .irq_stack = (char *) 553664832 /*0x21004140*/,
            .current = (struct k_thread *) 553651864 /*0x21000e98*/,
            .idle_thread = (struct k_thread *) 553651680 /*0x21000de0*/,
            .slice_ticks = (int) 0,
            .id = (uint8_t) 0 /*0x0*/,
            /* .arch is Missing */
        },
    },
    .ready_q = (struct _ready_q) {
        .cache = (struct k_thread *) 553651864 /*0x21000e98*/,
        .runq = (sys_dlist_t) {
            .head = (struct _dnode *) 553651864 /*0x21000e98*/,
            .next = (struct _dnode *) 553651864 /*0x21000e98*/,
            .tail = (struct _dnode *) 553650440 /*0x21000908*/,
            .prev = (struct _dnode *) 553650440 /*0x21000908*/,
        },
    },
    .threads = (struct k_thread *) 553652056 /*0x21000f58*/,
}
```

### Postfix Operators

Once a global is selected, you may follow members with postfix operators.
All postfix operators expect a type class (struct, array, pointer, etc.), and
will error when the type at that step of the query is of a different class.

The operators are:
 * `.<struct-member>` to traverse a struct into a member named
   "struct-member". This will derefrence pointers to structures
   before atempting to move to a member if the current type is a 
   pointer to a structure. This will error when the struct does
   not contain the required member.
 * `[<number>]` will select array member with index "number". Similar
   to the struct member operation, this will dereference pointers to
   arrays before atempting to select the member. This will error if
   the index is out of bounds, or the array has no size.
 * `[]` will select all array members, treating the remainder of the
   query as a query over all members of the array. This will error if
   the array has no size.
 * `.*` postfix derefrence operator. This will derefrence a pointer.


#### Struct Member

Continuing with the Zephyr kernel structure, we may select the head of the
threads linked list as follows:

```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.threads
21001ee4: (struct k_thread *) 553652056 /*0x21000f58*/
```

If we were to have a typo, we would get an error such as:
```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.thread
Error:
  × member not found "thread"
   ┌─[command-line:1:1]
 1 │ zephyr::_kernel.thread
   ·                 ───┬──
   ·                    └── missing
   └────
  help: consider replacing with "cpus", "ready_q" or "threads" instead
```

If we try to select a member of something that's not a struct, such as the cpu
array, we get a type error such as:
```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.cpus.nested
Error:
  × type mismatch
   ┌─[command-line:1:1]
 1 │ zephyr::_kernel.cpus.nested
   ·                      ───┬──
   ·                         └── expected struct or union found struct _cpu[1]
   └────
```

Members may be selected through poiters to structures, without an intevening 
dereference operator. 
For exmple, if we wanted to select the `arch` member of the thread struct at
the head of the threads linked list, we may use the following query:
```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.threads
21001ee4: (struct k_thread *) 553652056 /*0x21000f58*/
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.threads.arch
21001004: (struct _thread_arch) {
    .basepri = (uint32_t) 0 /*0x0*/,
    .swap_return_value = (uint32_t) 4294967285 /*0xfffffff5*/,
    .mode = (uint32_t) 48128 /*0xbc00*/,
    .mode_bits = (uint8_t) 0 /*0x0*/,
    .mode_exc_return = (uint8_t) 188 /*0xbc*/,
    .mode_reserved2 = (uint16_t) 0 /*0x0*/,
}
```
Note that above the type of `zephyr::_kernel.threads` is a `struct k_thread *`,
but we used it as if it were a `struct k_thread`.

### Array Index

Selecting an index in an array works similar to selecting a struct member.
For example if we wanted to select the first entry (index 0) of the Zephyr
kernel's cpu array we may:
```
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.cpus[0]
21001ec0: (struct _cpu) {
    .nested = (uint32_t) 0 /*0x0*/,
    .irq_stack = (char *) 553664832 /*0x21004140*/,
    .current = (struct k_thread *) 553651864 /*0x21000e98*/,
    .idle_thread = (struct k_thread *) 553651680 /*0x21000de0*/,
    .slice_ticks = (int) 0,
    .id = (uint8_t) 0 /*0x0*/,
    /* .arch is Missing */
}
```

However, if we index past the end of the array, we get an error:
```
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.cpus[11110]
Error:
  × Array index out of bounds
   ┌─[command-line:1:1]
 1 │ zephyr::_kernel.cpus[11110]
   ·                     ───────
   └────
  help: This array has a size of 1
```

Further if we try to index something other than an array, we get a type error
such as:
```
$ aerology query dhcpv4_client.core.0  zephyr::_kernel[6]
Error:
  × type mismatch
   ┌─[command-line:1:1]
 1 │ zephyr::_kernel[6]
   ·                ─┬─
   ·                 └── expected array found struct z_kernel
   └────
```

### Whole Array

You may select a whole array by omitting an array index. For example, we may
print the value of the char array that makes up a thread's name with the query:
```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.threads.name[]
21000fd0: (char) 115 /*0x73*/
21000fd1: (char) 121 /*0x79*/
21000fd2: (char) 115 /*0x73*/
21000fd3: (char) 119 /*0x77*/
21000fd4: (char) 111 /*0x6f*/
21000fd5: (char) 114 /*0x72*/
21000fd6: (char) 107 /*0x6b*/
21000fd7: (char) 113 /*0x71*/
21000fd8: (char) 0 /*0x0*/
21000fd9: (char) 0 /*0x0*/
21000fda: (char) 0 /*0x0*/
21000fdb: (char) 0 /*0x0*/
21000fdc: (char) 0 /*0x0*/
21000fdd: (char) 0 /*0x0*/
21000fde: (char) 0 /*0x0*/
21000fdf: (char) 0 /*0x0*/
21000fe0: (char) 0 /*0x0*/
21000fe1: (char) 0 /*0x0*/
21000fe2: (char) 0 /*0x0*/
21000fe3: (char) 0 /*0x0*/
21000fe4: (char) 0 /*0x0*/
21000fe5: (char) 0 /*0x0*/
21000fe6: (char) 0 /*0x0*/
21000fe7: (char) 0 /*0x0*/
21000fe8: (char) 0 /*0x0*/
21000fe9: (char) 0 /*0x0*/
21000fea: (char) 0 /*0x0*/
21000feb: (char) 0 /*0x0*/
21000fec: (char) 0 /*0x0*/
21000fed: (char) 0 /*0x0*/
21000fee: (char) 0 /*0x0*/
21000fef: (char) 0 /*0x0*/
```

This query is a tad silly, as aerology is capable of printing strings directly:
```bash
$ aerology query dhcpv4_client.core.0  zephyr::_kernel.threads.name
21000fd0: (char[32]) "sysworkq"
```

## Filters

At the moment, 4 filters are supported:
 * Postfix operators.
 * Reading a linked list with `llnodes`
 * Creating a bactrace with `bt`
 * Casting, with intrusive struct type checking

### Postfix operators

Postfix operators may be used as a filter by themselves.
Using an example from the prior section, we may query the name of the head of
the thread list including the filter separator `|` in a few more places.

```bash
$ aerology query dhcpv4_client.core.0  'zephyr::_kernel | .threads.name'
21000fd0: (char[32]) "sysworkq"

$ aerology  query dhcpv4_client.core.0  'zephyr::_kernel | .threads | .name'
21000fd0: (char[32]) "sysworkq"

$ aerology query dhcpv4_client.core.0  'zephyr::_kernel.threads | .name'
o21000fd0: (char[32]) "sysworkq"
```

As may be seen in the exmaple above, these produce the same result.

### Linked List Nodes

An important filter in Aerology is the linked list nodes reader.
It's invoked `llnodes <postfix-member>`.
For example, to get the names of all of the threads in the system, we may
use the following query:

```bash
$ aerology query dhcpv4_client.core.0 'zephyr::_kernel.threads | llnodes .next_thread | .name'
21000e58: (char[32]) "idle 00"
210008c8: (char[32]) "logging"
21000f10: (char[32]) "main"
21000980: (char[32]) "shell_uart"
21000fd0: (char[32]) "sysworkq"
```

### Backtrace

Another interesting Aerology filter is the backtrace filter.
This filter has a more complex syntax:
```
bt <reg-name>=<postfix-member>
```

This filter backtraces after populating the initial register state as
described by its arguments.

For example, we can take a backtrace of every thread in TFM with the
following query:
```
$ aerology query dhcpv4_client.core.0 'tfm_s::partition_listhead | llnodes .next | bt pc=.ctx_ctrl.exc_ret psp_s=.ctx_ctrl.sp'
3000bfcc:
  ╞═ Exception Handler Called
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
3000c014:
  ╞═ Exception Handler Called
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
3000c05c:
  ╞═ Exception Handler Called
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
3000c0a4:
  ╞═ Exception Handler Called
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
3000c0ec:
  ╞═ Exception Handler Called
  ├─ 11008e70 in tfm_arch_trigger_pendsv
  ├─ 11007ee3 in spm_interface_thread_dispatcher
  └─ 11021187 in psa_interface_unified_abi
3000c134:
  ╞═ Exception Handler Called
  └─ 11008c85 in tfm_nspm_thread_entry
3000c6e4:
  ╞═ Exception Handler Called
  └─ 11008ca5 in tfm_idle_thread
```

> Note: Zephyr does not store the entire exception payload, so without bitwise
> operations, we cannot construct a valid value for the pc.

> Note: we place the exception payload in pc, not the lr.

### Cast

The cast filter allows type conversion, or convert to an outer type of an
intrusive data structures.

Cast has the form:

```
cast (<type>)<optional-postfix-member>
```

without the postfix member, a simple cast is performed, converting the type of
the query to the one specified in the cast filter.

For example, to get the name of the head of the thread in the run queue:

```
$ aerology query dhcpv4_client.core.4 '_kernel.ready_q.runq.head.next | cast (struct k_thread) | .name'
21000f10: (char[32]) "main"
```

With a postfix member, an intrusive data structure cast is performed. An
intrusive cast provides type checking, and automatic dereferencing.
An intrusive cast treats the type as the destination type and the postfix member
as the sorce type, using the postfix member to compute the offset within the
destination type.
Further, the source type is checked with the type of the input.
If the type of the source is dereferenceable (in one step) from the input type
the derefrence is performed.
If the type of the source is does not match or is derefrenceable from the input
type, then a type error is raised.

For example, the same query as before, using an intrusive cast:
```
$ aerology query dhcpv4_client.core.4 '_kernel.ready_q.runq.head | cast (struct k_thread).base.qnode_dlist | .name'
21000f10: (char[32]) "main"
```

> Note: the above query includes a derefrence as part of the cast filter.

And if you omit the head member, creating a type error:

```
$ target/debug/aerology query dhcpv4_client.core.4 '_kernel.ready_q | cast (struct k_thread).base.qnode_dlist | .name'
Error:
  × type mismatch
   ┌─[command-line:1:1]
 1 │ _kernel.ready_q | cast (struct k_thread).base.qnode_dlist | .name
   ·                                         ────────┬────────
   ·                                                 └── expected struct _dnode found struct _ready_q
   └────
```
