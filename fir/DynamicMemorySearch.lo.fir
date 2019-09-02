circuit DynamicMemorySearch : @[:@2.0]
  module DynamicMemorySearch : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_isWr : UInt<1> @[:@6.4]
    input io_wrAddr : UInt<3> @[:@6.4]
    input io_data : UInt<4> @[:@6.4]
    input io_en : UInt<1> @[:@6.4]
    output io_target : UInt<3> @[:@6.4]
    output io_done : UInt<1> @[:@6.4]
  
    mem list : @[DynamicMemorySearch.scala 32:19:@9.4]
      data-type => UInt<4>
      depth => 8
      read-latency => 0
      write-latency => 1
      reader => memVal
      writer => _T_27
      read-under-write => undefined
    reg index : UInt<3>, clock with :
      reset => (UInt<1>("h0"), index) @[DynamicMemorySearch.scala 30:23:@8.4]
    node _T_22 = eq(io_en, UInt<1>("h0")) @[DynamicMemorySearch.scala 35:16:@11.4]
    node _T_23 = eq(list.memVal.data, io_data) @[DynamicMemorySearch.scala 35:35:@12.4]
    node _T_25 = eq(index, UInt<3>("h7")) @[DynamicMemorySearch.scala 35:58:@13.4]
    node _T_26 = or(_T_23, _T_25) @[DynamicMemorySearch.scala 35:48:@14.4]
    node done = and(_T_22, _T_26) @[DynamicMemorySearch.scala 35:23:@15.4]
    node _T_30 = eq(done, UInt<1>("h0")) @[DynamicMemorySearch.scala 41:21:@25.8]
    node _T_32 = add(index, UInt<1>("h1")) @[DynamicMemorySearch.scala 42:20:@27.10]
    node _T_33 = tail(_T_32, 1) @[DynamicMemorySearch.scala 42:20:@28.10]
    node _GEN_0 = mux(_T_30, _T_33, index) @[DynamicMemorySearch.scala 41:34:@26.8]
    node _GEN_1 = mux(io_en, UInt<1>("h0"), _GEN_0) @[DynamicMemorySearch.scala 39:23:@21.6]
    node _GEN_2 = validif(io_isWr, io_wrAddr) @[DynamicMemorySearch.scala 37:18:@16.4]
    node _GEN_3 = validif(io_isWr, clock) @[DynamicMemorySearch.scala 37:18:@16.4]
    node _GEN_4 = mux(io_isWr, UInt<1>("h1"), UInt<1>("h0")) @[DynamicMemorySearch.scala 37:18:@16.4]
    node _GEN_5 = validif(io_isWr, UInt<1>("h1")) @[DynamicMemorySearch.scala 37:18:@16.4]
    node _GEN_6 = validif(io_isWr, io_data) @[DynamicMemorySearch.scala 37:18:@16.4]
    node _GEN_7 = mux(io_isWr, index, _GEN_1) @[DynamicMemorySearch.scala 37:18:@16.4]
    io_target <= index @[DynamicMemorySearch.scala 45:13:@32.4]
    io_done <= done @[DynamicMemorySearch.scala 44:13:@31.4]
    index <= mux(reset, UInt<3>("h0"), _GEN_7) @[DynamicMemorySearch.scala 40:11:@22.8 DynamicMemorySearch.scala 42:11:@29.10]
    list.memVal.addr <= index @[DynamicMemorySearch.scala 33:20:@10.4]
    list.memVal.en <= UInt<1>("h1") @[DynamicMemorySearch.scala 32:19:@9.4 DynamicMemorySearch.scala 33:20:@10.4]
    list.memVal.clk <= clock @[DynamicMemorySearch.scala 33:20:@10.4]
    list._T_27.addr <= _GEN_2 @[DynamicMemorySearch.scala 38:9:@17.6]
    list._T_27.en <= _GEN_4 @[DynamicMemorySearch.scala 32:19:@9.4 DynamicMemorySearch.scala 38:9:@17.6]
    list._T_27.clk <= _GEN_3 @[DynamicMemorySearch.scala 38:9:@17.6]
    list._T_27.data <= _GEN_6 @[DynamicMemorySearch.scala 38:21:@18.6]
    list._T_27.mask <= _GEN_5 @[DynamicMemorySearch.scala 38:9:@17.6 DynamicMemorySearch.scala 38:21:@18.6]
