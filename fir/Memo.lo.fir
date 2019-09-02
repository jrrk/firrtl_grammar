circuit Memo : @[:@2.0]
  module Memo : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_wen : UInt<1> @[:@6.4]
    input io_wrAddr : UInt<8> @[:@6.4]
    input io_wrData : UInt<8> @[:@6.4]
    input io_ren : UInt<1> @[:@6.4]
    input io_rdAddr : UInt<8> @[:@6.4]
    output io_rdData : UInt<8> @[:@6.4]
  
    mem mem : @[Memo.scala 23:16:@8.4]
      data-type => UInt<8>
      depth => 256
      read-latency => 0
      write-latency => 1
      reader => _T_21
      writer => _T_19
      read-under-write => undefined
    node _GEN_0 = validif(io_wen, io_wrAddr) @[Memo.scala 26:17:@9.4]
    node _GEN_1 = validif(io_wen, clock) @[Memo.scala 26:17:@9.4]
    node _GEN_2 = mux(io_wen, UInt<1>("h1"), UInt<1>("h0")) @[Memo.scala 26:17:@9.4]
    node _GEN_3 = validif(io_wen, UInt<1>("h1")) @[Memo.scala 26:17:@9.4]
    node _GEN_4 = validif(io_wen, io_wrData) @[Memo.scala 26:17:@9.4]
    node _GEN_5 = validif(io_ren, io_rdAddr) @[Memo.scala 30:17:@14.4]
    node _GEN_6 = validif(io_ren, clock) @[Memo.scala 30:17:@14.4]
    node _GEN_7 = mux(io_ren, UInt<1>("h1"), UInt<1>("h0")) @[Memo.scala 30:17:@14.4]
    node _GEN_8 = mux(io_ren, mem._T_21.data, UInt<1>("h0")) @[Memo.scala 30:17:@14.4]
    io_rdData <= _GEN_8 @[Memo.scala 29:13:@13.4 Memo.scala 30:29:@16.6]
    mem._T_21.addr <= _GEN_5 @[Memo.scala 30:35:@15.6]
    mem._T_21.en <= _GEN_7 @[Memo.scala 23:16:@8.4 Memo.scala 30:35:@15.6]
    mem._T_21.clk <= _GEN_6 @[Memo.scala 30:35:@15.6]
    mem._T_19.addr <= _GEN_0 @[Memo.scala 26:22:@10.6]
    mem._T_19.en <= _GEN_2 @[Memo.scala 23:16:@8.4 Memo.scala 26:22:@10.6]
    mem._T_19.clk <= _GEN_1 @[Memo.scala 26:22:@10.6]
    mem._T_19.data <= _GEN_4 @[Memo.scala 26:34:@11.6]
    mem._T_19.mask <= _GEN_3 @[Memo.scala 26:22:@10.6 Memo.scala 26:34:@11.6]
