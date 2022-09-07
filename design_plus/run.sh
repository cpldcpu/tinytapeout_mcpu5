
iverilog -o mcpu5plus.vvp mcpu5plus.v mcpu5plus_tb.v assembler/rom_$1.v
vvp mcpu5plus.vvp

# Commment out synthtest to synthesize design
# yosys synthtest.ys | grep -i 'Printing' -A 28

# Comment out to automatically start gtkwave
# gtkwave mcpu5_tb.vcd gtkwave_settings.gtkw
