;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit ShiftRegister : 
  module ShiftRegister : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<1>, out : UInt<1>}
    
    reg r0 : UInt, clock @[ShiftRegister.scala 11:19]
    r0 <= io.in @[ShiftRegister.scala 11:19]
    reg r1 : UInt, clock @[ShiftRegister.scala 12:19]
    r1 <= r0 @[ShiftRegister.scala 12:19]
    reg r2 : UInt, clock @[ShiftRegister.scala 13:19]
    r2 <= r1 @[ShiftRegister.scala 13:19]
    reg r3 : UInt, clock @[ShiftRegister.scala 14:19]
    r3 <= r2 @[ShiftRegister.scala 14:19]
    io.out <= r3 @[ShiftRegister.scala 15:10]
    
