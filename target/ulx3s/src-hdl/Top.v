`default_nettype none

module Top (input wire  clk_25mhz,
            input wire  ftdi_txd,
            output wire ftdi_rxd,
	    output wire wifi_gpio0);
   
   // Tie gpio0 high, this keeps the board from rebooting
   assign wifi_gpio0 = 1'b1;

   wire CLK_100MHZ, CLK_250MHZ;
   wire reset;
   
   pll u_pll(
       .clkin_25mhz(clk_25mhz),
       .clkout_100mhz(CLK_100MHZ),
       .locked(reset)      
       );

   // // A reset line that goes low after 16 ticks
   // reg [2:0]                  reset_cnt = 0;
   // wire                       reset = ~reset_cnt[2];
   // always @(posedge clk_25mhz)
   //   if (reset) reset_cnt <= reset_cnt + 1;

  topEntity u_topEntity
    (.CLK_100MHZ(CLK_100MHZ),
     .RESET(reset),
     .RX(ftdi_txd),
     .TX(ftdi_rxd)
     );

endmodule
