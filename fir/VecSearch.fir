;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit VecSearch : 
  module VecSearch : 
    input clock : Clock
    input reset : UInt<1>
    output io : {out : UInt<4>}
    
    reg index : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[VecSearch.scala 14:22]
    wire elts : UInt<4>[7] @[VecSearch.scala 15:22]
    elts[0] <= UInt<4>("h00") @[VecSearch.scala 15:22]
    elts[1] <= UInt<4>("h04") @[VecSearch.scala 15:22]
    elts[2] <= UInt<4>("h0f") @[VecSearch.scala 15:22]
    elts[3] <= UInt<4>("h0e") @[VecSearch.scala 15:22]
    elts[4] <= UInt<4>("h02") @[VecSearch.scala 15:22]
    elts[5] <= UInt<4>("h05") @[VecSearch.scala 15:22]
    elts[6] <= UInt<4>("h0d") @[VecSearch.scala 15:22]
    node _T_29 = add(index, UInt<1>("h01")) @[VecSearch.scala 16:18]
    node _T_30 = tail(_T_29, 1) @[VecSearch.scala 16:18]
    index <= _T_30 @[VecSearch.scala 16:9]
    io.out <= elts[index] @[VecSearch.scala 17:10]
    
