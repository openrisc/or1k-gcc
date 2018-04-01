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
- branches - DONE
- tuning
  - ensure varargs are to spec
  - ensure we can return 64-bit values in r11 and r12
  - proper support for returning small structs/vectors in regs
  - optional support for frame pointers
  - delay slot
  - optional support for delay slot
  - correct predicates and constraints

## building

### Stage 1 build

```
mkdir build
cd build
../gcc/configure --target=or1k-elf --disable-shared --enable-stage1-languages=c --prefix=/home/shorne/work/gnu-toolchain/local
make -j5 all-gcc
make install-gcc

# Currently the entire build will not complete, but gcc will work, you can then
# test gcc using one of the below tests
../local/bin/or1k-elf-gcc -S ../gcc/test2.c -fdump-rtl-all

```

## bootstrap tests

Below are some really basic test programs we can compile before we move on to
the testsuite to check things.  I pulled these from the ggx toolchain porting
tutorial.

### Verify types
```
/* Just test some types */
    int myint;
    short myshort;
    double mydouble;
```

### Verify call/return
```
    int foo(int, int);
    int main()
    {
       return foo (111, 222);
    }
```

### Verify prolog/epilog
```

    int g;

    int add(int a, int b, int c, int d, int e, int f)
    {
      return a + b + c + d + e + f + g;
    }

    int main()
    {
      g = 7;
      return (add (1, 2, 3, 4, 5, 6));
    }
```
