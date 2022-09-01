
iverilog -o MCPU5.vvp user_module_341528610027340372.v user_module_341528610027340372_tb.v
vvp MCPU5.vvp
yosys synthtest.ys | grep -i 'Printing' -A 28
gtkwave.exe MCPU5_tb.vcd gtkwave_settings.gtkw
