;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Parity : 
  module Parity : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<1>, out : UInt<1>}
    
    reg state : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Parity.scala 13:23]
    when io.in : @[Parity.scala 14:16]
      node _T_10 = eq(state, UInt<1>("h00")) @[Parity.scala 15:17]
      when _T_10 : @[Parity.scala 15:29]
        state <= UInt<1>("h01") @[Parity.scala 15:37]
        skip @[Parity.scala 15:29]
      else : @[Parity.scala 16:29]
        state <= UInt<1>("h00") @[Parity.scala 16:37]
        skip @[Parity.scala 16:29]
      skip @[Parity.scala 14:16]
    node _T_11 = eq(state, UInt<1>("h01")) @[Parity.scala 18:20]
    io.out <= _T_11 @[Parity.scala 18:10]
    
