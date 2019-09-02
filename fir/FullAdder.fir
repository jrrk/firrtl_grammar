;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit FullAdder : 
  module FullAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
