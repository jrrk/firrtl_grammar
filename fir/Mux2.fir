;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Mux2 : 
  module Mux2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip sel : UInt<1>, flip in0 : UInt<1>, flip in1 : UInt<1>, out : UInt<1>}
    
    node _T_13 = and(io.sel, io.in1) @[Mux4.scala 18:21]
    node _T_14 = not(io.sel) @[Mux4.scala 18:34]
    node _T_15 = and(_T_14, io.in0) @[Mux4.scala 18:42]
    node _T_16 = or(_T_13, _T_15) @[Mux4.scala 18:31]
    io.out <= _T_16 @[Mux4.scala 18:10]
    
