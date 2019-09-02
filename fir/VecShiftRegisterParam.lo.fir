circuit VecShiftRegisterParam : @[:@2.0]
  module VecShiftRegisterParam : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : UInt<4> @[:@6.4]
    output io_out : UInt<4> @[:@6.4]
  
    reg delays_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_0) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_1) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_2) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_3) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_4) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_5) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_6) @[VecShiftRegisterParam.scala 19:23:@17.4]
    reg delays_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_7) @[VecShiftRegisterParam.scala 19:23:@17.4]
    node _T_12_0 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@9.4]
    node _T_12_1 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@10.4]
    node _T_12_2 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@11.4]
    node _T_12_3 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@12.4]
    node _T_12_4 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@13.4]
    node _T_12_5 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@14.4]
    node _T_12_6 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@15.4]
    node _T_12_7 = UInt<4>("h0") @[VecShiftRegisterParam.scala 19:31:@8.4 VecShiftRegisterParam.scala 19:31:@16.4]
    io_out <= delays_7 @[VecShiftRegisterParam.scala 26:10:@26.4]
    delays_0 <= mux(reset, _T_12_0, io_in) @[VecShiftRegisterParam.scala 25:13:@25.4]
    delays_1 <= mux(reset, _T_12_1, delays_0) @[VecShiftRegisterParam.scala 22:15:@24.4]
    delays_2 <= mux(reset, _T_12_2, delays_1) @[VecShiftRegisterParam.scala 22:15:@23.4]
    delays_3 <= mux(reset, _T_12_3, delays_2) @[VecShiftRegisterParam.scala 22:15:@22.4]
    delays_4 <= mux(reset, _T_12_4, delays_3) @[VecShiftRegisterParam.scala 22:15:@21.4]
    delays_5 <= mux(reset, _T_12_5, delays_4) @[VecShiftRegisterParam.scala 22:15:@20.4]
    delays_6 <= mux(reset, _T_12_6, delays_5) @[VecShiftRegisterParam.scala 22:15:@19.4]
    delays_7 <= mux(reset, _T_12_7, delays_6) @[VecShiftRegisterParam.scala 22:15:@18.4]
