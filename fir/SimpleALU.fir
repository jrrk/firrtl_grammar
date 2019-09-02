;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit SimpleALU : 
  module SimpleALU : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<4>, flip b : UInt<4>, flip opcode : UInt<2>, out : UInt<4>}
    
    io.out <= UInt<1>("h00") @[SimpleALU.scala 44:10]
    node _T_15 = eq(io.opcode, UInt<1>("h00")) @[SimpleALU.scala 45:19]
    when _T_15 : @[SimpleALU.scala 45:28]
      node _T_16 = add(io.a, io.b) @[SimpleALU.scala 46:20]
      node _T_17 = tail(_T_16, 1) @[SimpleALU.scala 46:20]
      io.out <= _T_17 @[SimpleALU.scala 46:12]
      skip @[SimpleALU.scala 45:28]
    else : @[SimpleALU.scala 47:35]
      node _T_19 = eq(io.opcode, UInt<1>("h01")) @[SimpleALU.scala 47:26]
      when _T_19 : @[SimpleALU.scala 47:35]
        node _T_20 = sub(io.a, io.b) @[SimpleALU.scala 48:20]
        node _T_21 = asUInt(_T_20) @[SimpleALU.scala 48:20]
        node _T_22 = tail(_T_21, 1) @[SimpleALU.scala 48:20]
        io.out <= _T_22 @[SimpleALU.scala 48:12]
        skip @[SimpleALU.scala 47:35]
      else : @[SimpleALU.scala 49:35]
        node _T_24 = eq(io.opcode, UInt<2>("h02")) @[SimpleALU.scala 49:26]
        when _T_24 : @[SimpleALU.scala 49:35]
          io.out <= io.a @[SimpleALU.scala 50:12]
          skip @[SimpleALU.scala 49:35]
        else : @[SimpleALU.scala 51:16]
          io.out <= io.b @[SimpleALU.scala 52:12]
          skip @[SimpleALU.scala 51:16]
    
