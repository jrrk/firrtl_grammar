;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit DynamicMemorySearch : 
  module DynamicMemorySearch : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<4>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
    
    reg index : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[DynamicMemorySearch.scala 30:23]
    cmem list : UInt<4>[8] @[DynamicMemorySearch.scala 32:19]
    infer mport memVal = list[index], clock @[DynamicMemorySearch.scala 33:20]
    node _T_22 = eq(io.en, UInt<1>("h00")) @[DynamicMemorySearch.scala 35:16]
    node _T_23 = eq(memVal, io.data) @[DynamicMemorySearch.scala 35:35]
    node _T_25 = eq(index, UInt<3>("h07")) @[DynamicMemorySearch.scala 35:58]
    node _T_26 = or(_T_23, _T_25) @[DynamicMemorySearch.scala 35:48]
    node done = and(_T_22, _T_26) @[DynamicMemorySearch.scala 35:23]
    when io.isWr : @[DynamicMemorySearch.scala 37:18]
      infer mport _T_27 = list[io.wrAddr], clock @[DynamicMemorySearch.scala 38:9]
      _T_27 <= io.data @[DynamicMemorySearch.scala 38:21]
      skip @[DynamicMemorySearch.scala 37:18]
    else : @[DynamicMemorySearch.scala 39:23]
      when io.en : @[DynamicMemorySearch.scala 39:23]
        index <= UInt<1>("h00") @[DynamicMemorySearch.scala 40:11]
        skip @[DynamicMemorySearch.scala 39:23]
      else : @[DynamicMemorySearch.scala 41:34]
        node _T_30 = eq(done, UInt<1>("h00")) @[DynamicMemorySearch.scala 41:21]
        when _T_30 : @[DynamicMemorySearch.scala 41:34]
          node _T_32 = add(index, UInt<1>("h01")) @[DynamicMemorySearch.scala 42:20]
          node _T_33 = tail(_T_32, 1) @[DynamicMemorySearch.scala 42:20]
          index <= _T_33 @[DynamicMemorySearch.scala 42:11]
          skip @[DynamicMemorySearch.scala 41:34]
    io.done <= done @[DynamicMemorySearch.scala 44:13]
    io.target <= index @[DynamicMemorySearch.scala 45:13]
    
