;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Darken : 
  module Darken : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<8>, out : UInt<8>}
    
    node _T_10 = dshl(io.in, UInt<1>("h01")) @[Darken.scala 12:19]
    io.out <= _T_10 @[Darken.scala 12:10]
    
