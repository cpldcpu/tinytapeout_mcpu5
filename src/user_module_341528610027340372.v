
`default_nettype none

module user_module_341528610027340372(
  input [7:0] io_in,
  output [7:0] io_out
);

MCPU5 MCPU5_top (
  .clk(io_in[0]),
  .rst(io_in[1]),
  .inst_in(io_in[7:2]),
  .cpu_out(io_out[7:0])
);

endmodule


module MCPU5(inst_in,cpu_out,rst,clk);

input [5:0] inst_in;
output [7:0] cpu_out;
input rst;
input clk;

localparam OP_JCC = 2'b00;      //00IIII
localparam OP_STA = 3'b101;     //101RRR


reg [8:0] accu; // accu(6) is carry !
reg [7:0] pc;
reg [7:0] regfile [0:8];
integer i;

initial begin
end

//    handle regfile writes (STA)
    always @(*)
        if (rst) begin
            for (i=0; i<=8; i=i+1)
                regfile[i] <=0;
        end
        else if ((inst_in[5:3] == OP_STA) && ~rst && ~clk)
            regfile[inst_in[2:0]] <= accu;
       
	always @(posedge clk)
		if (rst) begin
			accu <= 0;	
			pc <= 0;
            // for (i=0; i<=8; i=i+1)
            //     regfile[i] <=0;
		end
		else begin
            // Register file, handle STA
            // if (inst_in[5:3] == OP_STA)
            //     regfile[inst_in[2:0]] <= accu[7:0];

            // PC path
            if ((inst_in[5:4] == OP_JCC) && ~accu[8])
                pc <= { pc[7:4], inst_in[3:0]};   // TODO: Handle segmented jump
            else
                pc <= pc + 1'b1;

            // ALU path
            casex(inst_in)
                6'b00????: accu[8]   <= 1'b0;                                              // JCC #imm4
                6'b01????: accu[7:0] <= {{4{inst_in[3]}}, inst_in[3:0]}  ;                 // LDI #simm4
                6'b100???: accu[8:0] <= {1'b0,regfile[inst_in[2:0]]} + {1'b0,accu[7:0]};   // ADD reg8
                6'b101???: ;                                                               // STA reg8
                6'b110???: ;                                                               // free reg 8
                6'b111000: accu[7:0] <= ~accu[7:0];                                        // NOT
                6'b111001: ;                                                               // OUT
                6'b111010: ;                                                               // Free 
                6'b111011: ;                                                               // Free
                6'b1111??: ;                                                               // Free imm2
            endcase
		end

assign cpu_out = clk ? pc[7:0] :  accu[7:0] ; 

endmodule