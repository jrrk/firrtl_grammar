circuit Risc : @[:@2.0]
  module Risc : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_isWr : UInt<1> @[:@6.4]
    input io_wrAddr : UInt<8> @[:@6.4]
    input io_wrData : UInt<32> @[:@6.4]
    input io_boot : UInt<1> @[:@6.4]
    output io_valid : UInt<1> @[:@6.4]
    output io_out : UInt<32> @[:@6.4]
  
    mem file : @[Risc.scala 16:17:@8.4]
      data-type => UInt<32>
      depth => 256
      read-latency => 0
      write-latency => 1
      reader => _T_26
      reader => _T_30
      writer => _T_47
      read-under-write => undefined
    mem code : @[Risc.scala 17:17:@9.4]
      data-type => UInt<32>
      depth => 256
      read-latency => 0
      write-latency => 1
      reader => inst
      writer => _T_35
      read-under-write => undefined
    reg pc : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pc) @[Risc.scala 18:21:@10.4]
    node op = bits(code.inst.data, 31, 24) @[Risc.scala 23:18:@12.4]
    node rci = bits(code.inst.data, 23, 16) @[Risc.scala 24:18:@13.4]
    node rai = bits(code.inst.data, 15, 8) @[Risc.scala 25:18:@14.4]
    node rbi = bits(code.inst.data, 7, 0) @[Risc.scala 26:18:@15.4]
    node _T_24 = eq(rai, UInt<1>("h0")) @[Risc.scala 28:20:@16.4]
    node ra = mux(_T_24, UInt<1>("h0"), file._T_26.data) @[Risc.scala 28:15:@18.4]
    node _T_28 = eq(rbi, UInt<1>("h0")) @[Risc.scala 29:20:@19.4]
    node rb = mux(_T_28, UInt<1>("h0"), file._T_30.data) @[Risc.scala 29:15:@21.4]
    node _T_37 = eq(UInt<1>("h0"), op) @[Conditional.scala 37:30:@35.8]
    node _T_38 = add(ra, rb) @[Risc.scala 42:29:@37.10]
    node _T_39 = tail(_T_38, 1) @[Risc.scala 42:29:@38.10]
    node _T_40 = eq(UInt<1>("h1"), op) @[Conditional.scala 37:30:@42.10]
    node _T_42 = dshl(rai, UInt<4>("h8")) @[Risc.scala 43:31:@44.12]
    node _T_43 = or(_T_42, rbi) @[Risc.scala 43:39:@45.12]
    node _GEN_0 = mux(_T_40, _T_43, UInt<1>("h0")) @[Conditional.scala 39:67:@43.10]
    node _GEN_1 = mux(_T_37, _T_39, _GEN_0) @[Conditional.scala 40:58:@36.8]
    node _T_45 = eq(rci, UInt<8>("hff")) @[Risc.scala 46:15:@49.8]
    node _GEN_2 = mux(_T_45, UInt<1>("h1"), UInt<1>("h0")) @[Risc.scala 46:26:@50.8]
    node _GEN_3 = validif(eq(_T_45, UInt<1>("h0")), rci) @[Risc.scala 46:26:@50.8]
    node _GEN_4 = validif(eq(_T_45, UInt<1>("h0")), clock) @[Risc.scala 46:26:@50.8]
    node _GEN_5 = mux(_T_45, UInt<1>("h0"), UInt<1>("h1")) @[Risc.scala 46:26:@50.8]
    node _GEN_6 = validif(eq(_T_45, UInt<1>("h0")), UInt<1>("h1")) @[Risc.scala 46:26:@50.8]
    node _GEN_9 = mux(io_boot, UInt<1>("h0"), _GEN_1) @[Risc.scala 38:25:@31.6]
    node _GEN_23 = mux(io_isWr, UInt<1>("h0"), _GEN_9) @[Risc.scala 36:18:@26.4]
    node rc = _GEN_23 @[Risc.scala 30:16:@22.4 Risc.scala 34:12:@25.4 Risc.scala 42:23:@39.10 Risc.scala 43:23:@46.12]
    node _GEN_7 = validif(eq(_T_45, UInt<1>("h0")), rc) @[Risc.scala 46:26:@50.8]
    node _T_49 = add(pc, UInt<1>("h1")) @[Risc.scala 51:14:@57.8]
    node _T_50 = tail(_T_49, 1) @[Risc.scala 51:14:@58.8]
    node _GEN_8 = mux(io_boot, UInt<1>("h0"), _T_50) @[Risc.scala 38:25:@31.6]
    node _GEN_10 = mux(io_boot, UInt<1>("h0"), rc) @[Risc.scala 38:25:@31.6]
    node _GEN_11 = mux(io_boot, UInt<1>("h0"), _GEN_2) @[Risc.scala 38:25:@31.6]
    node _GEN_12 = validif(eq(io_boot, UInt<1>("h0")), _GEN_3) @[Risc.scala 38:25:@31.6]
    node _GEN_13 = validif(eq(io_boot, UInt<1>("h0")), _GEN_4) @[Risc.scala 38:25:@31.6]
    node _GEN_14 = mux(io_boot, UInt<1>("h0"), _GEN_5) @[Risc.scala 38:25:@31.6]
    node _GEN_15 = validif(eq(io_boot, UInt<1>("h0")), _GEN_6) @[Risc.scala 38:25:@31.6]
    node _GEN_16 = validif(eq(io_boot, UInt<1>("h0")), _GEN_7) @[Risc.scala 38:25:@31.6]
    node _GEN_17 = validif(io_isWr, io_wrAddr) @[Risc.scala 36:18:@26.4]
    node _GEN_18 = validif(io_isWr, clock) @[Risc.scala 36:18:@26.4]
    node _GEN_19 = mux(io_isWr, UInt<1>("h1"), UInt<1>("h0")) @[Risc.scala 36:18:@26.4]
    node _GEN_20 = validif(io_isWr, UInt<1>("h1")) @[Risc.scala 36:18:@26.4]
    node _GEN_21 = validif(io_isWr, io_wrData) @[Risc.scala 36:18:@26.4]
    node _GEN_22 = mux(io_isWr, pc, _GEN_8) @[Risc.scala 36:18:@26.4]
    node _GEN_24 = mux(io_isWr, UInt<1>("h0"), _GEN_10) @[Risc.scala 36:18:@26.4]
    node _GEN_25 = mux(io_isWr, UInt<1>("h0"), _GEN_11) @[Risc.scala 36:18:@26.4]
    node _GEN_26 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_12) @[Risc.scala 36:18:@26.4]
    node _GEN_27 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_13) @[Risc.scala 36:18:@26.4]
    node _GEN_28 = mux(io_isWr, UInt<1>("h0"), _GEN_14) @[Risc.scala 36:18:@26.4]
    node _GEN_29 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_15) @[Risc.scala 36:18:@26.4]
    node _GEN_30 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_16) @[Risc.scala 36:18:@26.4]
    io_valid <= _GEN_25 @[Risc.scala 32:12:@23.4 Risc.scala 47:16:@51.10]
    io_out <= _GEN_24 @[Risc.scala 33:12:@24.4 Risc.scala 45:12:@48.8]
    file._T_26.addr <= rai @[Risc.scala 28:38:@17.4]
    file._T_26.en <= UInt<1>("h1") @[Risc.scala 16:17:@8.4 Risc.scala 28:38:@17.4]
    file._T_26.clk <= clock @[Risc.scala 28:38:@17.4]
    file._T_30.addr <= rbi @[Risc.scala 29:38:@20.4]
    file._T_30.en <= UInt<1>("h1") @[Risc.scala 16:17:@8.4 Risc.scala 29:38:@20.4]
    file._T_30.clk <= clock @[Risc.scala 29:38:@20.4]
    file._T_47.addr <= _GEN_26 @[Risc.scala 49:11:@54.10]
    file._T_47.en <= _GEN_28 @[Risc.scala 16:17:@8.4 Risc.scala 49:11:@54.10]
    file._T_47.clk <= _GEN_27 @[Risc.scala 49:11:@54.10]
    file._T_47.data <= _GEN_30 @[Risc.scala 49:17:@55.10]
    file._T_47.mask <= _GEN_29 @[Risc.scala 49:11:@54.10 Risc.scala 49:17:@55.10]
    code.inst.addr <= pc @[Risc.scala 22:18:@11.4]
    code.inst.en <= UInt<1>("h1") @[Risc.scala 17:17:@9.4 Risc.scala 22:18:@11.4]
    code.inst.clk <= clock @[Risc.scala 22:18:@11.4]
    code._T_35.addr <= _GEN_17 @[Risc.scala 37:9:@27.6]
    code._T_35.en <= _GEN_19 @[Risc.scala 17:17:@9.4 Risc.scala 37:9:@27.6]
    code._T_35.clk <= _GEN_18 @[Risc.scala 37:9:@27.6]
    code._T_35.data <= _GEN_21 @[Risc.scala 37:21:@28.6]
    code._T_35.mask <= _GEN_20 @[Risc.scala 37:9:@27.6 Risc.scala 37:21:@28.6]
    pc <= mux(reset, UInt<8>("h0"), _GEN_22) @[Risc.scala 39:8:@32.8 Risc.scala 51:8:@59.8]
