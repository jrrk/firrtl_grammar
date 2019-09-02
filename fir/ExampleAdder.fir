;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Adder : 
  module FullAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module FullAdder_7 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    node a_xor_b = xor(io.a, io.b) @[FullAdder.scala 16:22]
    node _T_15 = xor(a_xor_b, io.cin) @[FullAdder.scala 17:21]
    io.sum <= _T_15 @[FullAdder.scala 17:10]
    node a_and_b = and(io.a, io.b) @[FullAdder.scala 19:22]
    node b_and_cin = and(io.b, io.cin) @[FullAdder.scala 20:24]
    node a_and_cin = and(io.a, io.cin) @[FullAdder.scala 21:24]
    node _T_16 = or(a_and_b, b_and_cin) @[FullAdder.scala 22:22]
    node _T_17 = or(_T_16, a_and_cin) @[FullAdder.scala 22:34]
    io.cout <= _T_17 @[FullAdder.scala 22:11]
    
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<8>, flip B : UInt<8>, flip Cin : UInt<1>, Sum : UInt<8>, Cout : UInt<1>}
    
    inst FullAdder of FullAdder @[Adder.scala 19:35]
    FullAdder.clock <= clock
    FullAdder.reset <= reset
    inst FullAdder_1 of FullAdder_1 @[Adder.scala 19:35]
    FullAdder_1.clock <= clock
    FullAdder_1.reset <= reset
    inst FullAdder_2 of FullAdder_2 @[Adder.scala 19:35]
    FullAdder_2.clock <= clock
    FullAdder_2.reset <= reset
    inst FullAdder_3 of FullAdder_3 @[Adder.scala 19:35]
    FullAdder_3.clock <= clock
    FullAdder_3.reset <= reset
    inst FullAdder_4 of FullAdder_4 @[Adder.scala 19:35]
    FullAdder_4.clock <= clock
    FullAdder_4.reset <= reset
    inst FullAdder_5 of FullAdder_5 @[Adder.scala 19:35]
    FullAdder_5.clock <= clock
    FullAdder_5.reset <= reset
    inst FullAdder_6 of FullAdder_6 @[Adder.scala 19:35]
    FullAdder_6.clock <= clock
    FullAdder_6.reset <= reset
    inst FullAdder_7 of FullAdder_7 @[Adder.scala 19:35]
    FullAdder_7.clock <= clock
    FullAdder_7.reset <= reset
    wire carry : UInt<1>[9] @[Adder.scala 20:19]
    wire sum : UInt<1>[8] @[Adder.scala 21:19]
    carry[0] <= io.Cin @[Adder.scala 24:12]
    node _T_42 = bits(io.A, 0, 0) @[Adder.scala 28:21]
    FullAdder.io.a <= _T_42 @[Adder.scala 28:14]
    node _T_43 = bits(io.B, 0, 0) @[Adder.scala 29:21]
    FullAdder.io.b <= _T_43 @[Adder.scala 29:14]
    FullAdder.io.cin <= carry[0] @[Adder.scala 30:16]
    carry[1] <= FullAdder.io.cout @[Adder.scala 31:16]
    node _T_44 = bits(FullAdder.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[0] <= _T_44 @[Adder.scala 32:12]
    node _T_45 = bits(io.A, 1, 1) @[Adder.scala 28:21]
    FullAdder_1.io.a <= _T_45 @[Adder.scala 28:14]
    node _T_46 = bits(io.B, 1, 1) @[Adder.scala 29:21]
    FullAdder_1.io.b <= _T_46 @[Adder.scala 29:14]
    FullAdder_1.io.cin <= carry[1] @[Adder.scala 30:16]
    carry[2] <= FullAdder_1.io.cout @[Adder.scala 31:16]
    node _T_47 = bits(FullAdder_1.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[1] <= _T_47 @[Adder.scala 32:12]
    node _T_48 = bits(io.A, 2, 2) @[Adder.scala 28:21]
    FullAdder_2.io.a <= _T_48 @[Adder.scala 28:14]
    node _T_49 = bits(io.B, 2, 2) @[Adder.scala 29:21]
    FullAdder_2.io.b <= _T_49 @[Adder.scala 29:14]
    FullAdder_2.io.cin <= carry[2] @[Adder.scala 30:16]
    carry[3] <= FullAdder_2.io.cout @[Adder.scala 31:16]
    node _T_50 = bits(FullAdder_2.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[2] <= _T_50 @[Adder.scala 32:12]
    node _T_51 = bits(io.A, 3, 3) @[Adder.scala 28:21]
    FullAdder_3.io.a <= _T_51 @[Adder.scala 28:14]
    node _T_52 = bits(io.B, 3, 3) @[Adder.scala 29:21]
    FullAdder_3.io.b <= _T_52 @[Adder.scala 29:14]
    FullAdder_3.io.cin <= carry[3] @[Adder.scala 30:16]
    carry[4] <= FullAdder_3.io.cout @[Adder.scala 31:16]
    node _T_53 = bits(FullAdder_3.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[3] <= _T_53 @[Adder.scala 32:12]
    node _T_54 = bits(io.A, 4, 4) @[Adder.scala 28:21]
    FullAdder_4.io.a <= _T_54 @[Adder.scala 28:14]
    node _T_55 = bits(io.B, 4, 4) @[Adder.scala 29:21]
    FullAdder_4.io.b <= _T_55 @[Adder.scala 29:14]
    FullAdder_4.io.cin <= carry[4] @[Adder.scala 30:16]
    carry[5] <= FullAdder_4.io.cout @[Adder.scala 31:16]
    node _T_56 = bits(FullAdder_4.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[4] <= _T_56 @[Adder.scala 32:12]
    node _T_57 = bits(io.A, 5, 5) @[Adder.scala 28:21]
    FullAdder_5.io.a <= _T_57 @[Adder.scala 28:14]
    node _T_58 = bits(io.B, 5, 5) @[Adder.scala 29:21]
    FullAdder_5.io.b <= _T_58 @[Adder.scala 29:14]
    FullAdder_5.io.cin <= carry[5] @[Adder.scala 30:16]
    carry[6] <= FullAdder_5.io.cout @[Adder.scala 31:16]
    node _T_59 = bits(FullAdder_5.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[5] <= _T_59 @[Adder.scala 32:12]
    node _T_60 = bits(io.A, 6, 6) @[Adder.scala 28:21]
    FullAdder_6.io.a <= _T_60 @[Adder.scala 28:14]
    node _T_61 = bits(io.B, 6, 6) @[Adder.scala 29:21]
    FullAdder_6.io.b <= _T_61 @[Adder.scala 29:14]
    FullAdder_6.io.cin <= carry[6] @[Adder.scala 30:16]
    carry[7] <= FullAdder_6.io.cout @[Adder.scala 31:16]
    node _T_62 = bits(FullAdder_6.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[6] <= _T_62 @[Adder.scala 32:12]
    node _T_63 = bits(io.A, 7, 7) @[Adder.scala 28:21]
    FullAdder_7.io.a <= _T_63 @[Adder.scala 28:14]
    node _T_64 = bits(io.B, 7, 7) @[Adder.scala 29:21]
    FullAdder_7.io.b <= _T_64 @[Adder.scala 29:14]
    FullAdder_7.io.cin <= carry[7] @[Adder.scala 30:16]
    carry[8] <= FullAdder_7.io.cout @[Adder.scala 31:16]
    node _T_65 = bits(FullAdder_7.io.sum, 0, 0) @[Adder.scala 32:32]
    sum[7] <= _T_65 @[Adder.scala 32:12]
    node _T_66 = cat(sum[1], sum[0]) @[Adder.scala 34:17]
    node _T_67 = cat(sum[3], sum[2]) @[Adder.scala 34:17]
    node _T_68 = cat(_T_67, _T_66) @[Adder.scala 34:17]
    node _T_69 = cat(sum[5], sum[4]) @[Adder.scala 34:17]
    node _T_70 = cat(sum[7], sum[6]) @[Adder.scala 34:17]
    node _T_71 = cat(_T_70, _T_69) @[Adder.scala 34:17]
    node _T_72 = cat(_T_71, _T_68) @[Adder.scala 34:17]
    io.Sum <= _T_72 @[Adder.scala 34:10]
    io.Cout <= carry[8] @[Adder.scala 35:11]
    
