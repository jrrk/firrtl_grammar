;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Counter : 
  module Counter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inc : UInt<1>, flip amt : UInt<4>, tot : UInt<8>}
    
    reg _T_14 : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[Counter.scala 18:20]
    when io.inc : @[Counter.scala 19:15]
      node _T_15 = add(_T_14, io.amt) @[Counter.scala 19:35]
      node _T_16 = tail(_T_15, 1) @[Counter.scala 19:35]
      node _T_17 = gt(_T_16, UInt<8>("h0ff")) @[Counter.scala 15:11]
      node _T_19 = mux(_T_17, UInt<1>("h00"), _T_16) @[Counter.scala 15:8]
      _T_14 <= _T_19 @[Counter.scala 19:19]
      skip @[Counter.scala 19:15]
    io.tot <= _T_14 @[Counter.scala 32:10]
    
