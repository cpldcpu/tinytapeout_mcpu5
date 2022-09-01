`timescale 1ns / 1ps

module MCPU5_tb;

wire [7:0] cpu_out;

reg clk, reset;
reg [5:0] inst_in;

// assign io_in = {inst_inputn, reset, clk};

user_module_341528610027340372 MCPU5_top ({inst_in,reset,clk},cpu_out);

initial begin
  $dumpfile("MCPU5_tb.vcd");
  $dumpvars(0, MCPU5_tb);
end

initial begin
   #1000; // Wait a long time in simulation units (adjust as needed).
   $display("Caught by trap");
   $finish;
 end

parameter CLK_HALF_PERIOD = 5;
parameter TCLK = 2*CLK_HALF_PERIOD;
always begin
    clk = 1'b1;
    #(CLK_HALF_PERIOD);
    clk = 1'b0;
    #(CLK_HALF_PERIOD);
end

initial 
begin
    inst_in = 6'b111001;  // OUT
    #(CLK_HALF_PERIOD);
    reset = 1;
    #(TCLK);
    reset = 0;
    #(TCLK);
    inst_in = 6'b010001;  // LDI #1
    #(TCLK);
    inst_in = 6'b011110;  // LDI #-2
    #(TCLK);
    inst_in = 6'b101001;  // STA R1 (-2)
    #(TCLK);
    inst_in = 6'b111000;  // NOT
    #(TCLK);
    inst_in = 6'b101010;  // STA R2 (1)
    #(TCLK);
    inst_in = 6'b010000;  // LDI #0 (a=0)
    #(TCLK);
    inst_in = 6'b100001;  // ADD R1 (a=-2)
    #(TCLK);
    inst_in = 6'b100010;  // ADD R2 (a=-1)
    #(TCLK);
    inst_in = 6'b100001;  // ADD R1 (a=-3,c=1)
    #(TCLK);
    inst_in = 6'b000010;  // JCC 2
    #(TCLK);
    inst_in = 6'b001111;  // JCC 4
    #(TCLK);
    inst_in = 6'b010001;  // LDI #1
    #(TCLK);
    inst_in = 6'b111010;  // JMPA
    #(TCLK);

end


endmodule
