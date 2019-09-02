;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit ByteSelector : 
  module ByteSelector : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<32>, flip offset : UInt<2>, out : UInt<8>}
    
    io.out <= UInt<8>("h00") @[ByteSelector.scala 12:10]
    node _T_13 = eq(io.offset, UInt<2>("h00")) @[ByteSelector.scala 13:19]
    when _T_13 : @[ByteSelector.scala 13:33]
      node _T_14 = bits(io.in, 7, 0) @[ByteSelector.scala 14:20]
      io.out <= _T_14 @[ByteSelector.scala 14:12]
      skip @[ByteSelector.scala 13:33]
    else : @[ByteSelector.scala 15:35]
      node _T_16 = eq(io.offset, UInt<1>("h01")) @[ByteSelector.scala 15:26]
      when _T_16 : @[ByteSelector.scala 15:35]
        node _T_17 = bits(io.in, 15, 8) @[ByteSelector.scala 16:20]
        io.out <= _T_17 @[ByteSelector.scala 16:12]
        skip @[ByteSelector.scala 15:35]
      else : @[ByteSelector.scala 17:35]
        node _T_19 = eq(io.offset, UInt<2>("h02")) @[ByteSelector.scala 17:26]
        when _T_19 : @[ByteSelector.scala 17:35]
          node _T_20 = bits(io.in, 23, 16) @[ByteSelector.scala 18:20]
          io.out <= _T_20 @[ByteSelector.scala 18:12]
          skip @[ByteSelector.scala 17:35]
        else : @[ByteSelector.scala 19:16]
          node _T_21 = bits(io.in, 31, 24) @[ByteSelector.scala 20:20]
          io.out <= _T_21 @[ByteSelector.scala 20:12]
          skip @[ByteSelector.scala 19:16]
    
