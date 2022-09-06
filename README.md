# Zinnia (MCPU5)

An 8 bit RISC CPU for [TinyTapeout](www.tinytapeout.com). Tinytapeout combines 500 designs on a single IC to be taped out with the Open MPW-7. This offers the opportunity to actually get a design made on a real IC, but also comes with some constraints:

- Maximum allowed area is 100 x 100 µm² in Skywater 130nm CMOS technology. The actual number of useable gates depends on cell size and routing limitations.
- Only eight digital inputs and eight digital outputs are allowed.
- I/O will be provided via the scanchain (a long shift register) and is hence rather slow.

Designing a CPU around these constraints offers a nice challenge. Challenge taken!

# Design Description

## Top level

The strict limitations on I/O do not allow implementing a normal interface with bidirectional data bus and separate address bus. One way of addressing this would be to reduce the data width of the CPU to 4 bit, but this was deemed to limiting. Another option, implementing a serial interface, appeared too slow and too complex.

Instead the I/Os were allocated as shown below.

<p align="center">
  <img width=400 src='https://user-images.githubusercontent.com/4086406/188716014-33053217-c1a6-4cac-afc2-257b7203d407.png'>
</p>

The CPU is based on the Harvard Architecture with separate data and program memories. The data memory is completely internal to the CPU. The program memory is external and is accessed through the I/O. All data has to be loaded as constants through machine code instructions.

Two of the input pins are used for clock and reset, the remaining ones are reserved for instructions and are six bit in length. The output is multiplexed between the program counter (when clk is '1') and the content of the main register, the Accumulator. Accessing the Accumulator allows reading the program output.

## Programmers Model

<p align="center">
  <img src='https://user-images.githubusercontent.com/4086406/188716065-a4d7755b-9020-4291-94e4-f22cf04bb168.png'>
</p>

Besides simplifying the external interface, the Harvard Architecture implementation also removes the requirement to interleave code and data access on the bus. Every instruction can be executed in a single clock cycle. Due to this, no state machine for micro-sequencing is required and instructions can be decoded directly from the inst[5:0] input.

All data operations are performed on the accumulator. In addition, there are eight data registers. The data registers are implemented as a single port memory based on latches, which significantly reduced are usage compared to a two port implementation. The Accu is complemented by a single carry flag, which can be used for conditional branches.

Handling of constants is supported by the integer flag („I-Flag“), which enables loading an eight bit constant with two consecutive 6 bit opcodes.

## Instruction Set Architecture

The list of instructions and their encoding is shown below. One challenge in the instruction set design was to encode the target address for branches. The limited opcode size only allows for a four bit immediate to be encoded as a maximum. Initially, I considered introducing an additional segment register for long jumps, but ultimately decided to introduce relative addressing for conditional branches and a long jmp instruction that is fed from the accumulator. 

Having both NOT and NEG may seems excessive, but the implementation was cheap on resources and some instruction sequences could be simplified.

No boolean logic instructions (AND/OR/NOT/NOR/XOR) are supported since they were not needed in any of my typical test programs.

![grafik](https://user-images.githubusercontent.com/4086406/188716202-d0681200-9578-414f-8c06-417b6ae8950d.png)

The table below shows common instruction sequences that can be realized with macros.

![grafik](https://user-images.githubusercontent.com/4086406/188716303-d0428667-788e-4f98-bd4b-40d5c7e23e4d.png)

## Design after placement and routing

The total cell count after synthesis is 489. Adding any additional features did not allow to complete the routing pass.

![grafik](https://user-images.githubusercontent.com/4086406/188730917-91d6c818-d903-449f-bae3-42abefd206a6.png)

![grafik](https://user-images.githubusercontent.com/4086406/188715948-98719648-8b37-4218-b3ca-6674cf783abc.png)


## Original TinyTapeout Readme
-----

![](../../workflows/wokwi/badge.svg)

Go to https://tinytapeout.com for instructions!

# How to change the Wokwi project

Edit the [Makefile](Makefile) and change the WOKWI_PROJECT_ID to match your project.

# What is this about?

This repo is a template you can make a copy of for your own [ASIC](https://www.zerotoasiccourse.com/terminology/asic/) design using [Wokwi](https://wokwi.com/).

When you edit the Makefile to choose a different ID, the [GitHub Action](.github/workflows/wokwi.yaml) will fetch the digital netlist of your design from Wokwi.

The design gets wrapped in some extra logic that builds a 'scan chain'. This is a way to put lots of designs onto one chip and still have access to them all. You can see [all of the technical details here](https://github.com/mattvenn/scan_wrapper).

After that, the action uses the open source ASIC tool called [OpenLane](https://www.zerotoasiccourse.com/terminology/openlane/) to build the files needed to fabricate an ASIC.

# What files get made?

When the action is complete, you can [click here](https://github.com/mattvenn/wokwi-verilog-gds-test/actions) to see the latest build of your design. You need to download the zip file and take a look at the contents:

* gds_render.svg - picture of your ASIC design
* gds.html - zoomable picture of your ASIC design
* runs/wokwi/reports/final_summary_report.csv  - CSV file with lots of details about the design
* runs/wokwi/reports/synthesis/1-synthesis.stat.rpt.strategy4 - list of the [standard cells](https://www.zerotoasiccourse.com/terminology/standardcell/) used by your design
* runs/wokwi/results/final/gds/user_module.gds - the final [GDS](https://www.zerotoasiccourse.com/terminology/gds2/) file needed to make your design

# What next?

* Share your GDS on twitter, tag it #tinytapeout and [link me](https://twitter.com/matthewvenn)!
* [Submit it to be made](https://docs.google.com/forms/d/e/1FAIpQLSc3ZF0AHKD3LoZRSmKX5byl-0AzrSK8ADeh0DtkZQX0bbr16w/viewform?usp=sf_link)
* [Join the community](https://discord.gg/rPK2nSjxy8)
