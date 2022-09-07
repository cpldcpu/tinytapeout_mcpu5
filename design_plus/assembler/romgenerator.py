# SMAL to verilog rom converter
#
# Usages: 
# a) - Python3 Conv_NCPU3.py <input.o >out.v

import sys

Memory  = [-1 for _ in range(0,256)]
PC      = 0
#   Read program (SMAL Output)

for currentline in sys.stdin.readlines():
    number=currentline.strip().rpartition('#')[2]
    cmd=currentline.strip()[0]
    if cmd == '.':
        try:
            PC = int(number,16)
        except:
            pass
    elif cmd == 'B':        
        Memory[PC]=int(number,16)
        PC = PC + 1

print(
'''
module instr_mem(
  input [7:0] address,
  output [5:0] instruction
);

reg [5:0] inst;
    always @(*) begin
        case(address)'''
    )   

for idx in range(0,256):
    if Memory[idx]!=-1:
        print('\t\t8\'h{:02X}:\tinst = 6\'h{:02X};\n'.format(idx,Memory[idx]),end='')

print(
'''\t\tdefault:\tinst = 6'b111111;
        endcase
    end

assign instruction = inst;
endmodule'''
)

