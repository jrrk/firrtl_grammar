circuit VecShiftRegister : @[:@2.0]
  module VecShiftRegister : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_ins_0 : UInt<4> @[:@6.4]
    input io_ins_1 : UInt<4> @[:@6.4]
    input io_ins_2 : UInt<4> @[:@6.4]
    input io_ins_3 : UInt<4> @[:@6.4]
    input io_load : UInt<1> @[:@6.4]
    input io_shift : UInt<1> @[:@6.4]
    output io_out : UInt<4> @[:@6.4]
  
    reg delays_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_0) @[VecShiftRegister.scala 20:19:@8.4]
    reg delays_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_1) @[VecShiftRegister.scala 20:19:@8.4]
    reg delays_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_2) @[VecShiftRegister.scala 20:19:@8.4]
    reg delays_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), delays_3) @[VecShiftRegister.scala 20:19:@8.4]
    node _GEN_0 = mux(io_shift, io_ins_0, delays_0) @[VecShiftRegister.scala 26:25:@16.6]
    node _GEN_1 = mux(io_shift, delays_0, delays_1) @[VecShiftRegister.scala 26:25:@16.6]
    node _GEN_2 = mux(io_shift, delays_1, delays_2) @[VecShiftRegister.scala 26:25:@16.6]
    node _GEN_3 = mux(io_shift, delays_2, delays_3) @[VecShiftRegister.scala 26:25:@16.6]
    node _GEN_4 = mux(io_load, io_ins_0, _GEN_0) @[VecShiftRegister.scala 21:18:@9.4]
    node _GEN_5 = mux(io_load, io_ins_1, _GEN_1) @[VecShiftRegister.scala 21:18:@9.4]
    node _GEN_6 = mux(io_load, io_ins_2, _GEN_2) @[VecShiftRegister.scala 21:18:@9.4]
    node _GEN_7 = mux(io_load, io_ins_3, _GEN_3) @[VecShiftRegister.scala 21:18:@9.4]
    io_out <= delays_3 @[VecShiftRegister.scala 32:10:@22.4]
    delays_0 <= _GEN_4 @[VecShiftRegister.scala 22:15:@10.6 VecShiftRegister.scala 27:15:@17.8]
    delays_1 <= _GEN_5 @[VecShiftRegister.scala 23:15:@11.6 VecShiftRegister.scala 28:15:@18.8]
    delays_2 <= _GEN_6 @[VecShiftRegister.scala 24:15:@12.6 VecShiftRegister.scala 29:15:@19.8]
    delays_3 <= _GEN_7 @[VecShiftRegister.scala 25:15:@13.6 VecShiftRegister.scala 30:15:@20.8]
