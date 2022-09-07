# Zinnia (MCPU5) Design Files

This folder contains cleaned up designfiles and testbench. The [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build) provides all tools required for compiling, synthesis and simulation of Zinnia.

After initializing the OSS CAD suite environment, you can execute the test bench by invoking the shell script ´run.sh´ 

Run fibonacci code 
> ./run.sh fibonacci

Run primes code
> ./run.sh primes

The output of the executed programs is directly displayed in the shell. In addition, a VCD file with waveforms is generated, which can be viewed with GTKWAVE or the [WaveTrace plugin](https://www.wavetrace.io/) in VSCode.

![grafik](https://user-images.githubusercontent.com/4086406/188920501-4257ff7e-d6df-495d-963a-cf1aee4d25f1.png)

The number in brackets shows the number of executed program cycles, the output shows the content of the accumulator when the "OUT" instruction in the machine code is executed.

![grafik](https://user-images.githubusercontent.com/4086406/188921127-461e2d08-ba10-4bcb-b11b-6f22f69dc238.png)

# Assembler

The subfolder [assembler/](assembler/) holds the SMAL32.c assembler, include files for MCPU5 and two assembler code examples (primes.asm and fibonacci.asm).

You can use the script ´do.sh´ to assemble files and convert them into a verilog ROM module to be used by the test bench.

Assemble fibonacci code:
>./assemble.sh fibonacci

Assemble primes code:
>./assemble.sh primes

A WIN32 executable compiled with MINGW32 is provided with SMAL32 along with the source. Warning: Unfortunately is appears that SMAL32.C is not compiled correctly under 64 bit GCC in Linux, although no error is shown at compile time.
