;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Combinational : 
  module Combinational : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip x : UInt<16>, flip y : UInt<16>, z : UInt<16>}
    
    node _T_11 = add(io.x, io.y) @[Combinational.scala 12:16]
    node _T_12 = tail(_T_11, 1) @[Combinational.scala 12:16]
    io.z <= _T_12 @[Combinational.scala 12:8]
    
