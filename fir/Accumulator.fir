;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Accumulator : 
  module Accumulator : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<1>, out : UInt<8>}
    
    reg accumulator : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[Accumulator.scala 16:28]
    node _T_11 = add(accumulator, io.in) @[Accumulator.scala 17:30]
    node _T_12 = tail(_T_11, 1) @[Accumulator.scala 17:30]
    accumulator <= _T_12 @[Accumulator.scala 17:15]
    io.out <= accumulator @[Accumulator.scala 18:10]
    
