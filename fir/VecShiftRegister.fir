;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit VecShiftRegister : 
  module VecShiftRegister : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip ins : UInt<4>[4], flip load : UInt<1>, flip shift : UInt<1>, out : UInt<4>}
    
    reg delays : UInt[4], clock @[VecShiftRegister.scala 20:19]
    when io.load : @[VecShiftRegister.scala 21:18]
      delays[0] <= io.ins[0] @[VecShiftRegister.scala 22:15]
      delays[1] <= io.ins[1] @[VecShiftRegister.scala 23:15]
      delays[2] <= io.ins[2] @[VecShiftRegister.scala 24:15]
      delays[3] <= io.ins[3] @[VecShiftRegister.scala 25:15]
      skip @[VecShiftRegister.scala 21:18]
    else : @[VecShiftRegister.scala 26:25]
      when io.shift : @[VecShiftRegister.scala 26:25]
        delays[0] <= io.ins[0] @[VecShiftRegister.scala 27:15]
        delays[1] <= delays[0] @[VecShiftRegister.scala 28:15]
        delays[2] <= delays[1] @[VecShiftRegister.scala 29:15]
        delays[3] <= delays[2] @[VecShiftRegister.scala 30:15]
        skip @[VecShiftRegister.scala 26:25]
    io.out <= delays[3] @[VecShiftRegister.scala 32:10]
    
