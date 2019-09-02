;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Adder : 
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : UInt<8>, flip in1 : UInt<8>, out : UInt<8>}
    
    node _T_11 = add(io.in0, io.in1) @[Adder.scala 17:20]
    node _T_12 = tail(_T_11, 1) @[Adder.scala 17:20]
    io.out <= _T_12 @[Adder.scala 17:10]
    
