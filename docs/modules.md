
where does module end and package start?

Modules
=======

All files in a directory, unless excluded, form a single namespace


example/src/
  file1.owl
  file2.owl
  old-file.owl
  some-data.txt
  manifest.yml (example.yml? module.yml? what name?)
example/build/<profile>/
  mycompany/
    example.db
    example.ll
    example.bc


modules-files = dir-files -exclude +include -target-exclude +target-include -feature-exclude +feature+include

config = requires, define, exclude, include
feature-config  config, reject

debug, release, unit-test = different build profiles

yaml lists capability objects in module?

---
mycompany.example:
  targets:      ; optional, default is "any"
    - target-triples
    - machine-vendor-operatingsystem
    - <arch><sub>-<vendor>-<sys>-<abi>
    - x86_64-linux-gnu
    - *-linux-gnu:
        include:
          - impl_linux.owl
    - riscv64-none-elf
  requires:     ; mandatory
    - stdlib
    - mycompany.stdlib +thing
  define:
    - VERSION: "1.0.0"
    - SOMETHING: 1
  exclude:
    - old-file.owl
    - impl_*.owl
  features:     ; optional
    big-thing:
      - feature-config
    little-thing:
      reject:
        - big-thing
      include:
        - thing.owl



mycompany.example+linux
