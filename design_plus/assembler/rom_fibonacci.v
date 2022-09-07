
module instr_mem(
  input [7:0] address,
  output [5:0] instruction
);

reg [5:0] inst;
    always @(*) begin
        case(address)
		8'h00:	inst = 6'h10;
		8'h01:	inst = 6'h28;
		8'h02:	inst = 6'h11;
		8'h03:	inst = 6'h29;
		8'h04:	inst = 6'h31;
		8'h05:	inst = 6'h2A;
		8'h06:	inst = 6'h20;
		8'h07:	inst = 6'h29;
		8'h08:	inst = 6'h32;
		8'h09:	inst = 6'h28;
		8'h0A:	inst = 6'h3B;
		8'h0B:	inst = 6'h09;
		8'h0C:	inst = 6'h00;
		default:	inst = 6'b111111;
        endcase
    end

assign instruction = inst;
endmodule
