circuit VecShiftRegisterSimple : @[:@2.0]
  module VecShiftRegisterSimple : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : UInt<8> @[:@6.4]
    output io_out : UInt<8> @[:@6.4]
  
    reg delays_0 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), delays_0) @[VecShiftRegisterSimple.scala 18:23:@13.4]
    reg delays_1 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), delays_1) @[VecShiftRegisterSimple.scala 18:23:@13.4]
    reg delays_2 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), delays_2) @[VecShiftRegisterSimple.scala 18:23:@13.4]
    reg delays_3 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), delays_3) @[VecShiftRegisterSimple.scala 18:23:@13.4]
    node _T_12_0 = UInt<8>("h0") @[VecShiftRegisterSimple.scala 18:31:@8.4 VecShiftRegisterSimple.scala 18:31:@9.4]
    node _T_12_1 = UInt<8>("h0") @[VecShiftRegisterSimple.scala 18:31:@8.4 VecShiftRegisterSimple.scala 18:31:@10.4]
    node _T_12_2 = UInt<8>("h0") @[VecShiftRegisterSimple.scala 18:31:@8.4 VecShiftRegisterSimple.scala 18:31:@11.4]
    node _T_12_3 = UInt<8>("h0") @[VecShiftRegisterSimple.scala 18:31:@8.4 VecShiftRegisterSimple.scala 18:31:@12.4]
    io_out <= delays_3 @[VecShiftRegisterSimple.scala 24:13:@18.4]
    delays_0 <= mux(reset, _T_12_0, io_in) @[VecShiftRegisterSimple.scala 20:13:@14.4]
    delays_1 <= mux(reset, _T_12_1, delays_0) @[VecShiftRegisterSimple.scala 21:13:@15.4]
    delays_2 <= mux(reset, _T_12_2, delays_1) @[VecShiftRegisterSimple.scala 22:13:@16.4]
    delays_3 <= mux(reset, _T_12_3, delays_2) @[VecShiftRegisterSimple.scala 23:13:@17.4]
