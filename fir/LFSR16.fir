;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit LFSR16 : 
  module LFSR16 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inc : UInt<1>, out : UInt<16>}
    
    reg res : UInt<16>, clock with : (reset => (reset, UInt<16>("h01"))) @[LFSR16.scala 19:20]
    when io.inc : @[LFSR16.scala 20:17]
      node _T_11 = bits(res, 0, 0) @[LFSR16.scala 21:26]
      node _T_12 = bits(res, 2, 2) @[LFSR16.scala 21:33]
      node _T_13 = xor(_T_11, _T_12) @[LFSR16.scala 21:29]
      node _T_14 = bits(res, 3, 3) @[LFSR16.scala 21:40]
      node _T_15 = xor(_T_13, _T_14) @[LFSR16.scala 21:36]
      node _T_16 = bits(res, 5, 5) @[LFSR16.scala 21:47]
      node _T_17 = xor(_T_15, _T_16) @[LFSR16.scala 21:43]
      node _T_18 = bits(res, 15, 1) @[LFSR16.scala 21:55]
      node _T_19 = cat(_T_17, _T_18) @[Cat.scala 30:58]
      res <= _T_19 @[LFSR16.scala 22:9]
      skip @[LFSR16.scala 20:17]
    io.out <= res @[LFSR16.scala 24:10]
    
