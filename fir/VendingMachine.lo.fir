circuit VendingMachine : @[:@2.0]
  module VendingMachine : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_nickel : UInt<1> @[:@6.4]
    input io_dime : UInt<1> @[:@6.4]
    output io_valid : UInt<1> @[:@6.4]
  
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[VendingMachine.scala 22:22:@8.4]
    node _T_12 = eq(state, UInt<3>("h0")) @[VendingMachine.scala 23:15:@9.4]
    node _GEN_0 = mux(io_nickel, UInt<3>("h1"), state) @[VendingMachine.scala 24:22:@11.6]
    node _GEN_1 = mux(io_dime, UInt<3>("h2"), _GEN_0) @[VendingMachine.scala 25:22:@14.6]
    node _GEN_2 = mux(_T_12, _GEN_1, state) @[VendingMachine.scala 23:26:@10.4]
    node _T_13 = eq(state, UInt<3>("h1")) @[VendingMachine.scala 27:15:@18.4]
    node _GEN_3 = mux(io_nickel, UInt<3>("h2"), _GEN_2) @[VendingMachine.scala 28:22:@20.6]
    node _GEN_4 = mux(io_dime, UInt<3>("h3"), _GEN_3) @[VendingMachine.scala 29:22:@23.6]
    node _GEN_5 = mux(_T_13, _GEN_4, _GEN_2) @[VendingMachine.scala 27:23:@19.4]
    node _T_14 = eq(state, UInt<3>("h2")) @[VendingMachine.scala 31:15:@27.4]
    node _GEN_6 = mux(io_nickel, UInt<3>("h3"), _GEN_5) @[VendingMachine.scala 32:22:@29.6]
    node _GEN_7 = mux(io_dime, UInt<3>("h4"), _GEN_6) @[VendingMachine.scala 33:22:@32.6]
    node _GEN_8 = mux(_T_14, _GEN_7, _GEN_5) @[VendingMachine.scala 31:24:@28.4]
    node _T_15 = eq(state, UInt<3>("h3")) @[VendingMachine.scala 35:15:@36.4]
    node _GEN_9 = mux(io_nickel, UInt<3>("h4"), _GEN_8) @[VendingMachine.scala 36:22:@38.6]
    node _GEN_10 = mux(io_dime, UInt<3>("h4"), _GEN_9) @[VendingMachine.scala 37:22:@41.6]
    node _GEN_11 = mux(_T_15, _GEN_10, _GEN_8) @[VendingMachine.scala 35:24:@37.4]
    node _T_16 = eq(state, UInt<3>("h4")) @[VendingMachine.scala 39:15:@45.4]
    node _GEN_12 = mux(_T_16, UInt<3>("h0"), _GEN_11) @[VendingMachine.scala 39:24:@46.4]
    node _T_17 = eq(state, UInt<3>("h4")) @[VendingMachine.scala 42:22:@49.4]
    io_valid <= _T_17 @[VendingMachine.scala 42:12:@50.4]
    state <= mux(reset, UInt<3>("h0"), _GEN_12) @[VendingMachine.scala 24:30:@12.8 VendingMachine.scala 25:30:@15.8 VendingMachine.scala 28:30:@21.8 VendingMachine.scala 29:30:@24.8 VendingMachine.scala 32:30:@30.8 VendingMachine.scala 33:30:@33.8 VendingMachine.scala 36:30:@39.8 VendingMachine.scala 37:30:@42.8 VendingMachine.scala 40:11:@47.6]
