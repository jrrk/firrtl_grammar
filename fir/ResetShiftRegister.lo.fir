circuit ResetShiftRegister : @[:@2.0]
  module ResetShiftRegister : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : UInt<4> @[:@6.4]
    input io_shift : UInt<1> @[:@6.4]
    output io_out : UInt<4> @[:@6.4]
  
    reg r0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), r0) @[ResetShiftRegister.scala 13:19:@8.4]
    reg r1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), r1) @[ResetShiftRegister.scala 14:19:@9.4]
    reg r2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), r2) @[ResetShiftRegister.scala 15:19:@10.4]
    reg r3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), r3) @[ResetShiftRegister.scala 16:19:@11.4]
    node _GEN_0 = mux(io_shift, io_in, r0) @[ResetShiftRegister.scala 17:19:@12.4]
    node _GEN_1 = mux(io_shift, r0, r1) @[ResetShiftRegister.scala 17:19:@12.4]
    node _GEN_2 = mux(io_shift, r1, r2) @[ResetShiftRegister.scala 17:19:@12.4]
    node _GEN_3 = mux(io_shift, r2, r3) @[ResetShiftRegister.scala 17:19:@12.4]
    io_out <= r3 @[ResetShiftRegister.scala 23:10:@18.4]
    r0 <= mux(reset, UInt<4>("h0"), _GEN_0) @[ResetShiftRegister.scala 18:8:@13.6]
    r1 <= mux(reset, UInt<4>("h0"), _GEN_1) @[ResetShiftRegister.scala 19:8:@14.6]
    r2 <= mux(reset, UInt<4>("h0"), _GEN_2) @[ResetShiftRegister.scala 20:8:@15.6]
    r3 <= mux(reset, UInt<4>("h0"), _GEN_3) @[ResetShiftRegister.scala 21:8:@16.6]
