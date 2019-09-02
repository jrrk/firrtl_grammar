;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit GCD : 
  module GCD : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<16>, flip b : UInt<16>, flip load : UInt<1>, out : UInt<16>, valid : UInt<1>}
    
    reg x : UInt, clock @[GCD.scala 20:14]
    reg y : UInt, clock @[GCD.scala 21:14]
    when io.load : @[GCD.scala 23:18]
      x <= io.a @[GCD.scala 24:7]
      y <= io.b @[GCD.scala 24:18]
      skip @[GCD.scala 23:18]
    else : @[GCD.scala 25:16]
      node _T_17 = gt(x, y) @[GCD.scala 26:13]
      when _T_17 : @[GCD.scala 26:18]
        node _T_18 = sub(x, y) @[GCD.scala 27:14]
        node _T_19 = asUInt(_T_18) @[GCD.scala 27:14]
        node _T_20 = tail(_T_19, 1) @[GCD.scala 27:14]
        x <= _T_20 @[GCD.scala 27:9]
        skip @[GCD.scala 26:18]
      else : @[GCD.scala 28:26]
        node _T_21 = leq(x, y) @[GCD.scala 28:20]
        when _T_21 : @[GCD.scala 28:26]
          node _T_22 = sub(y, x) @[GCD.scala 29:14]
          node _T_23 = asUInt(_T_22) @[GCD.scala 29:14]
          node _T_24 = tail(_T_23, 1) @[GCD.scala 29:14]
          y <= _T_24 @[GCD.scala 29:9]
          skip @[GCD.scala 28:26]
      skip @[GCD.scala 25:16]
    io.out <= x @[GCD.scala 33:10]
    node _T_26 = eq(y, UInt<1>("h00")) @[GCD.scala 34:17]
    io.valid <= _T_26 @[GCD.scala 34:12]
    
