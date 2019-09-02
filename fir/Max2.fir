;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Max2 : 
  module Max2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : UInt<8>, flip in1 : UInt<8>, out : UInt<8>}
    
    node _T_11 = gt(io.in0, io.in1) @[Max2.scala 17:24]
    node _T_12 = mux(_T_11, io.in0, io.in1) @[Max2.scala 17:16]
    io.out <= _T_12 @[Max2.scala 17:10]
    
