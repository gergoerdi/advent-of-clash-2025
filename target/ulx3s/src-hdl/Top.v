`default_nettype none

module Top (input wire  clk_25mhz,
            input wire  ftdi_txd,
            output wire ftdi_rxd,
	    output wire wifi_gpio0);
   
   // Tie gpio0 high, this keeps the board from rebooting
   assign wifi_gpio0 = 1'b1;

   wire                 clk_100mhz;
   wire                 reset;
   
   pll u_pll(
       .clkin_25mhz(clk_25mhz),
       .clkout_100mhz(clk_100mhz),
       .locked(reset)      
       );

  topEntity u_topEntity
    (.CLK_100MHZ(clk_100mhz),
     .RESET(!reset),
     .RX(ftdi_txd),
     .TX(ftdi_rxd)
     );

endmodule
