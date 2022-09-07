
iverilog -o mcpu5.vvp mcpu5.v mcpu5_tb.v assembler/rom_$1.v
vvp mcpu5.vvp

# Commment out synthtest to synthesize design
# yosys synthtest.ys | grep -i 'Printing' -A 28

# Comment out to automatically start gtkwave
# gtkwave mcpu5_tb.vcd gtkwave_settings.gtkw
