;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit ResetShiftRegister : 
  module ResetShiftRegister : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<4>, flip shift : UInt<1>, out : UInt<4>}
    
    reg r0 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[ResetShiftRegister.scala 13:19]
    reg r1 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[ResetShiftRegister.scala 14:19]
    reg r2 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[ResetShiftRegister.scala 15:19]
    reg r3 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[ResetShiftRegister.scala 16:19]
    when io.shift : @[ResetShiftRegister.scala 17:19]
      r0 <= io.in @[ResetShiftRegister.scala 18:8]
      r1 <= r0 @[ResetShiftRegister.scala 19:8]
      r2 <= r1 @[ResetShiftRegister.scala 20:8]
      r3 <= r2 @[ResetShiftRegister.scala 21:8]
      skip @[ResetShiftRegister.scala 17:19]
    io.out <= r3 @[ResetShiftRegister.scala 23:10]
    
