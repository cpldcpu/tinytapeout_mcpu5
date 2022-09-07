
module instr_mem(
  input [7:0] address,
  output [5:0] instruction
);

reg [5:0] inst;
    always @(*) begin
        case(address)
		8'h00:	inst = 6'h12;
		8'h01:	inst = 6'h28;
		8'h02:	inst = 6'h3B;
		8'h03:	inst = 6'h12;
		8'h04:	inst = 6'h29;
		8'h05:	inst = 6'h11;
		8'h06:	inst = 6'h20;
		8'h07:	inst = 6'h28;
		8'h08:	inst = 6'h30;
		8'h09:	inst = 6'h39;
		8'h0A:	inst = 6'h21;
		8'h0B:	inst = 6'h0F;
		8'h0C:	inst = 6'h39;
		8'h0D:	inst = 6'h03;
		8'h0E:	inst = 6'h14;
		8'h0F:	inst = 6'h0F;
		8'h10:	inst = 6'h11;
		8'h11:	inst = 6'h21;
		8'h12:	inst = 6'h29;
		8'h13:	inst = 6'h39;
		8'h14:	inst = 6'h20;
		8'h15:	inst = 6'h12;
		8'h16:	inst = 6'h0F;
		8'h17:	inst = 6'h30;
		8'h18:	inst = 6'h3B;
		8'h19:	inst = 6'h01;
		8'h1A:	inst = 6'h18;
		8'h1B:	inst = 6'h0E;
		default:	inst = 6'b111111;
        endcase
    end

assign instruction = inst;
endmodule
