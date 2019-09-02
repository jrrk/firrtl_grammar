;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit SingleEvenFilter : 
  module PredicateFilter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {valid : UInt<1>, bits : UInt<16>}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    node _T_18 = leq(io.in.bits, UInt<4>("h09")) @[SingleEvenFilter.scala 30:51]
    node _T_19 = and(io.in.valid, _T_18) @[SingleEvenFilter.scala 24:31]
    io.out.valid <= _T_19 @[SingleEvenFilter.scala 24:16]
    io.out.bits <= io.in.bits @[SingleEvenFilter.scala 25:16]
    
  module PredicateFilter_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {valid : UInt<1>, bits : UInt<16>}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    node _T_17 = bits(io.in.bits, 0, 0) @[SingleEvenFilter.scala 35:50]
    node _T_18 = bits(_T_17, 0, 0) @[SingleEvenFilter.scala 35:54]
    node _T_19 = and(io.in.valid, _T_18) @[SingleEvenFilter.scala 24:31]
    io.out.valid <= _T_19 @[SingleEvenFilter.scala 24:16]
    io.out.bits <= io.in.bits @[SingleEvenFilter.scala 25:16]
    
  module SingleEvenFilter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {valid : UInt<1>, bits : UInt<16>}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    inst single of PredicateFilter @[SingleEvenFilter.scala 30:11]
    single.clock <= clock
    single.reset <= reset
    inst even of PredicateFilter_1 @[SingleEvenFilter.scala 35:11]
    even.clock <= clock
    even.reset <= reset
    single.io.in.bits <= io.in.bits @[SingleEvenFilter.scala 41:17]
    single.io.in.valid <= io.in.valid @[SingleEvenFilter.scala 41:17]
    even.io.in.bits <= single.io.out.bits @[SingleEvenFilter.scala 42:17]
    even.io.in.valid <= single.io.out.valid @[SingleEvenFilter.scala 42:17]
    io.out.bits <= even.io.out.bits @[SingleEvenFilter.scala 43:17]
    io.out.valid <= even.io.out.valid @[SingleEvenFilter.scala 43:17]
    
