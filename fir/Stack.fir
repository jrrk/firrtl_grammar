;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Stack : 
  module Stack : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip push : UInt<1>, flip pop : UInt<1>, flip en : UInt<1>, flip dataIn : UInt<32>, dataOut : UInt<32>}
    
    cmem stack_mem : UInt<32>[8] @[Stack.scala 16:22]
    reg sp : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Stack.scala 17:26]
    reg out : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[Stack.scala 18:26]
    when io.en : @[Stack.scala 20:16]
      node _T_22 = lt(sp, UInt<4>("h08")) @[Stack.scala 21:25]
      node _T_23 = and(io.push, _T_22) @[Stack.scala 21:18]
      when _T_23 : @[Stack.scala 21:42]
        node _T_24 = bits(sp, 2, 0) @[Stack.scala 22:16]
        infer mport _T_25 = stack_mem[_T_24], clock @[Stack.scala 22:16]
        _T_25 <= io.dataIn @[Stack.scala 22:21]
        node _T_27 = add(sp, UInt<1>("h01")) @[Stack.scala 23:16]
        node _T_28 = tail(_T_27, 1) @[Stack.scala 23:16]
        sp <= _T_28 @[Stack.scala 23:10]
        skip @[Stack.scala 21:42]
      else : @[Stack.scala 24:39]
        node _T_30 = gt(sp, UInt<1>("h00")) @[Stack.scala 24:31]
        node _T_31 = and(io.pop, _T_30) @[Stack.scala 24:24]
        when _T_31 : @[Stack.scala 24:39]
          node _T_33 = sub(sp, UInt<1>("h01")) @[Stack.scala 25:16]
          node _T_34 = asUInt(_T_33) @[Stack.scala 25:16]
          node _T_35 = tail(_T_34, 1) @[Stack.scala 25:16]
          sp <= _T_35 @[Stack.scala 25:10]
          skip @[Stack.scala 24:39]
      node _T_37 = gt(sp, UInt<1>("h00")) @[Stack.scala 27:14]
      when _T_37 : @[Stack.scala 27:21]
        node _T_39 = sub(sp, UInt<1>("h01")) @[Stack.scala 28:27]
        node _T_40 = asUInt(_T_39) @[Stack.scala 28:27]
        node _T_41 = tail(_T_40, 1) @[Stack.scala 28:27]
        node _T_42 = bits(_T_41, 2, 0) @[Stack.scala 28:23]
        infer mport _T_43 = stack_mem[_T_42], clock @[Stack.scala 28:23]
        out <= _T_43 @[Stack.scala 28:11]
        skip @[Stack.scala 27:21]
      skip @[Stack.scala 20:16]
    io.dataOut <= out @[Stack.scala 32:14]
    
