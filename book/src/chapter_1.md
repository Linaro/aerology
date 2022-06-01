# Introduction

Aerology is a special purpose debugger for inspecting snapshots of
Zephyr and TFM Applications.

At the time of writing it is able to provide convinient access to:
 * memory layout, with the `segments` subcommand
 * Zephyr DTS, with the `dts` subcommand
 * Zephyr Config, with the `command` subcommand
 * stack usage by thread, with the `stacks` subcommand
 * backtraces by thread, with the `backtrace` subcommand
 * and general purpose queries beginning with global variables, with the `query` subcommand
