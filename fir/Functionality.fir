;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Functionality : 
  module Functionality : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip x : UInt<16>, flip y : UInt<16>, z : UInt<16>}
    
    node _T_11 = and(io.x, io.y) @[Functionality.scala 13:8]
    node _T_12 = not(io.x) @[Functionality.scala 13:16]
    node _T_13 = and(_T_12, io.y) @[Functionality.scala 13:19]
    node _T_14 = or(_T_11, _T_13) @[Functionality.scala 13:13]
    io.z <= _T_14 @[Functionality.scala 14:8]
    
