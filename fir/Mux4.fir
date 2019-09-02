;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Mux4 : 
  module Mux2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip sel : UInt<1>, flip in0 : UInt<1>, flip in1 : UInt<1>, out : UInt<1>}
    
    node _T_13 = and(io.sel, io.in1) @[Mux4.scala 18:21]
    node _T_14 = not(io.sel) @[Mux4.scala 18:34]
    node _T_15 = and(_T_14, io.in0) @[Mux4.scala 18:42]
    node _T_16 = or(_T_13, _T_15) @[Mux4.scala 18:31]
    io.out <= _T_16 @[Mux4.scala 18:10]
    
  module Mux2_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip sel : UInt<1>, flip in0 : UInt<1>, flip in1 : UInt<1>, out : UInt<1>}
    
    node _T_13 = and(io.sel, io.in1) @[Mux4.scala 18:21]
    node _T_14 = not(io.sel) @[Mux4.scala 18:34]
    node _T_15 = and(_T_14, io.in0) @[Mux4.scala 18:42]
    node _T_16 = or(_T_13, _T_15) @[Mux4.scala 18:31]
    io.out <= _T_16 @[Mux4.scala 18:10]
    
  module Mux2_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip sel : UInt<1>, flip in0 : UInt<1>, flip in1 : UInt<1>, out : UInt<1>}
    
    node _T_13 = and(io.sel, io.in1) @[Mux4.scala 18:21]
    node _T_14 = not(io.sel) @[Mux4.scala 18:34]
    node _T_15 = and(_T_14, io.in0) @[Mux4.scala 18:42]
    node _T_16 = or(_T_13, _T_15) @[Mux4.scala 18:31]
    io.out <= _T_16 @[Mux4.scala 18:10]
    
  module Mux4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : UInt<1>, flip in1 : UInt<1>, flip in2 : UInt<1>, flip in3 : UInt<1>, flip sel : UInt<2>, out : UInt<1>}
    
    inst m0 of Mux2 @[Mux4.scala 36:18]
    m0.clock <= clock
    m0.reset <= reset
    node _T_17 = bits(io.sel, 0, 0) @[Mux4.scala 37:22]
    m0.io.sel <= _T_17 @[Mux4.scala 37:13]
    m0.io.in0 <= io.in0 @[Mux4.scala 38:13]
    m0.io.in1 <= io.in1 @[Mux4.scala 39:13]
    inst m1 of Mux2_1 @[Mux4.scala 41:18]
    m1.clock <= clock
    m1.reset <= reset
    node _T_18 = bits(io.sel, 0, 0) @[Mux4.scala 42:22]
    m1.io.sel <= _T_18 @[Mux4.scala 42:13]
    m1.io.in0 <= io.in2 @[Mux4.scala 43:13]
    m1.io.in1 <= io.in3 @[Mux4.scala 44:13]
    inst m2 of Mux2_2 @[Mux4.scala 46:18]
    m2.clock <= clock
    m2.reset <= reset
    node _T_19 = bits(io.sel, 1, 1) @[Mux4.scala 47:22]
    m2.io.sel <= _T_19 @[Mux4.scala 47:13]
    m2.io.in0 <= m0.io.out @[Mux4.scala 48:13]
    m2.io.in1 <= m1.io.out @[Mux4.scala 49:13]
    io.out <= m2.io.out @[Mux4.scala 51:10]
    
