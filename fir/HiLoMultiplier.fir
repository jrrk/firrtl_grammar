;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit HiLoMultiplier : 
  module HiLoMultiplier : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<16>, flip B : UInt<16>, Hi : UInt<16>, Lo : UInt<16>}
    
    node mult = mul(io.A, io.B) @[HiLoMultiplier.scala 14:19]
    node _T_13 = bits(mult, 15, 0) @[HiLoMultiplier.scala 15:16]
    io.Lo <= _T_13 @[HiLoMultiplier.scala 15:9]
    node _T_14 = bits(mult, 31, 16) @[HiLoMultiplier.scala 16:16]
    io.Hi <= _T_14 @[HiLoMultiplier.scala 16:9]
    
