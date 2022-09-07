`timescale 1ns / 1ps

// MCPU5 testbench
// Program memory is in ROM module "instr_mem"


module MCPU5_tb;

wire [7:0] cpu_out;

reg clk, reset;
reg [5:0] inst_in;
reg [7:0] accuout;
wire [5:0] mem_out;
integer cycles = 0;

user_module_341528610027340372 MCPU5_top ({inst_in,reset,clk},cpu_out);
instr_mem mem_top(.address(cpu_out),.instruction(mem_out));

initial begin
  $dumpfile("MCPU5_tb.vcd");
  $dumpvars(0, MCPU5_tb);
end

initial begin
   #100000; // Wait a long time in simulation units (adjust as needed).
   $display("[%d] Cycle limit reached. Testbench aborted.",cycles);
   $finish;
 end

parameter CLK_HALF_PERIOD = 5;
parameter TCLK = 2*CLK_HALF_PERIOD;

always begin
    clk = 1'b1;
    #(1);
    inst_in=mem_out;
    #(CLK_HALF_PERIOD-1);
    clk = 1'b0;
    #(1);
    if (inst_in == 6'b111011)
      $display("[%d] Output: %d",cycles,cpu_out);   // output when OUT instruction was detected
    cycles = cycles + 1;
    #(CLK_HALF_PERIOD-1);
end

initial 
begin
    inst_in = 6'b111001;  // OUT
    #(CLK_HALF_PERIOD);
    reset = 1;
    #(TCLK);
    reset = 0;
    #(TCLK);
end


endmodule
