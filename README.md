# OpenRISC GCC port

this is the start of a new OpenRISC fsf clean port for gcc.

## Todo

These items should be done to get to a point where very basic things compile.
`DONE` - just means something is in place so we can move on not done and tested.

- build infra + basic files - DONE
- register definitions - DONE
- calling conventions - DONE
- memory layout
- load/store - DONE
- moves - DONE
- jump - DONE
- branches

## building

### Stage 1 build
../gcc/configure --target=or1k-elf --disable-shared --enable-stage1-languages=c --prefix=/home/shorne/work/gnu-toolchain/local
make -j5
