;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Risc : 
  module Risc : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<8>, flip wrData : UInt<32>, flip boot : UInt<1>, valid : UInt<1>, out : UInt<32>}
    
    cmem file : UInt<32>[256] @[Risc.scala 16:17]
    cmem code : UInt<32>[256] @[Risc.scala 17:17]
    reg pc : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[Risc.scala 18:21]
    infer mport inst = code[pc], clock @[Risc.scala 22:18]
    node op = bits(inst, 31, 24) @[Risc.scala 23:18]
    node rci = bits(inst, 23, 16) @[Risc.scala 24:18]
    node rai = bits(inst, 15, 8) @[Risc.scala 25:18]
    node rbi = bits(inst, 7, 0) @[Risc.scala 26:18]
    node _T_24 = eq(rai, UInt<1>("h00")) @[Risc.scala 28:20]
    infer mport _T_26 = file[rai], clock @[Risc.scala 28:38]
    node ra = mux(_T_24, UInt<1>("h00"), _T_26) @[Risc.scala 28:15]
    node _T_28 = eq(rbi, UInt<1>("h00")) @[Risc.scala 29:20]
    infer mport _T_30 = file[rbi], clock @[Risc.scala 29:38]
    node rb = mux(_T_28, UInt<1>("h00"), _T_30) @[Risc.scala 29:15]
    wire rc : UInt<32> @[Risc.scala 30:16]
    io.valid <= UInt<1>("h00") @[Risc.scala 32:12]
    io.out <= UInt<1>("h00") @[Risc.scala 33:12]
    rc <= UInt<1>("h00") @[Risc.scala 34:12]
    when io.isWr : @[Risc.scala 36:18]
      infer mport _T_35 = code[io.wrAddr], clock @[Risc.scala 37:9]
      _T_35 <= io.wrData @[Risc.scala 37:21]
      skip @[Risc.scala 36:18]
    else : @[Risc.scala 38:25]
      when io.boot : @[Risc.scala 38:25]
        pc <= UInt<1>("h00") @[Risc.scala 39:8]
        skip @[Risc.scala 38:25]
      else : @[Risc.scala 40:16]
        node _T_37 = eq(UInt<1>("h00"), op) @[Conditional.scala 37:30]
        when _T_37 : @[Conditional.scala 40:58]
          node _T_38 = add(ra, rb) @[Risc.scala 42:29]
          node _T_39 = tail(_T_38, 1) @[Risc.scala 42:29]
          rc <= _T_39 @[Risc.scala 42:23]
          skip @[Conditional.scala 40:58]
        else : @[Conditional.scala 39:67]
          node _T_40 = eq(UInt<1>("h01"), op) @[Conditional.scala 37:30]
          when _T_40 : @[Conditional.scala 39:67]
            node _T_42 = dshl(rai, UInt<4>("h08")) @[Risc.scala 43:31]
            node _T_43 = or(_T_42, rbi) @[Risc.scala 43:39]
            rc <= _T_43 @[Risc.scala 43:23]
            skip @[Conditional.scala 39:67]
        io.out <= rc @[Risc.scala 45:12]
        node _T_45 = eq(rci, UInt<8>("h0ff")) @[Risc.scala 46:15]
        when _T_45 : @[Risc.scala 46:26]
          io.valid <= UInt<1>("h01") @[Risc.scala 47:16]
          skip @[Risc.scala 46:26]
        else : @[Risc.scala 48:18]
          infer mport _T_47 = file[rci], clock @[Risc.scala 49:11]
          _T_47 <= rc @[Risc.scala 49:17]
          skip @[Risc.scala 48:18]
        node _T_49 = add(pc, UInt<1>("h01")) @[Risc.scala 51:14]
        node _T_50 = tail(_T_49, 1) @[Risc.scala 51:14]
        pc <= _T_50 @[Risc.scala 51:8]
        skip @[Risc.scala 40:16]
    
