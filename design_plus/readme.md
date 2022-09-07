# Zinnia+ (MCPU5_plus) Design Files

This folder contains an upgraded and modified version of MCPU5 to fix some of the issues that were encountered when porting more complex code. (Call it the post-tapeout-regret version.). The organization of this folder is the same as for the original design, so see there for updates.

Three changes have been introduced:

1)  Allowing the ```NEG``` instruction to upgrade the carry. This allows for an easy test for accu=0 or overflow during ```INC```/```DEC``` macros.

2) Modifying the ```BCC``` instruction to read part of the branch target address from the accu, if the iflag is set. This allows for much easier implementation of 8 bit branch target addresses.

3) Removing ```JMPA``` as it was deemed unnecessary with the modification above.

The resulting instruction set designs reduces code size, improves execution speed and even reduces the number of macroscells in the design. A clear win-win.

## Updated Instruction set
Changes highlighted in red
![grafik](https://user-images.githubusercontent.com/4086406/188964781-6225424b-dbec-4612-abec-af46d9475376.png)
## Macros using updated instruction set
![grafik](https://user-images.githubusercontent.com/4086406/188964855-a3e7898a-a654-4cfc-b8fc-e3d9afdaa0ac.png)
## Benchmark of normal vs. plus version
![grafik](https://user-images.githubusercontent.com/4086406/188964934-6237c9f9-7317-45d7-9a42-db6cfef9d972.png)


