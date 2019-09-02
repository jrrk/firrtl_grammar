;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit Adder4 : 
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
    
  module Adder4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip A : UInt<4>, flip B : UInt<4>, flip Cin : UInt<1>, Sum : UInt<4>, Cout : UInt<1>}
    
    inst Adder0 of FullAdder @[Adder4.scala 17:22]
    Adder0.clock <= clock
    Adder0.reset <= reset
    node _T_15 = bits(io.A, 0, 0) @[Adder4.scala 18:22]
    Adder0.io.a <= _T_15 @[Adder4.scala 18:15]
    node _T_16 = bits(io.B, 0, 0) @[Adder4.scala 19:22]
    Adder0.io.b <= _T_16 @[Adder4.scala 19:15]
    Adder0.io.cin <= io.Cin @[Adder4.scala 20:17]
    inst Adder1 of FullAdder_1 @[Adder4.scala 23:22]
    Adder1.clock <= clock
    Adder1.reset <= reset
    node _T_17 = bits(io.A, 1, 1) @[Adder4.scala 24:22]
    Adder1.io.a <= _T_17 @[Adder4.scala 24:15]
    node _T_18 = bits(io.B, 1, 1) @[Adder4.scala 25:22]
    Adder1.io.b <= _T_18 @[Adder4.scala 25:15]
    Adder1.io.cin <= Adder0.io.cout @[Adder4.scala 26:17]
    node s1 = cat(Adder1.io.sum, Adder0.io.sum) @[Cat.scala 30:58]
    inst Adder2 of FullAdder_2 @[Adder4.scala 29:22]
    Adder2.clock <= clock
    Adder2.reset <= reset
    node _T_19 = bits(io.A, 2, 2) @[Adder4.scala 30:22]
    Adder2.io.a <= _T_19 @[Adder4.scala 30:15]
    node _T_20 = bits(io.B, 2, 2) @[Adder4.scala 31:22]
    Adder2.io.b <= _T_20 @[Adder4.scala 31:15]
    Adder2.io.cin <= Adder1.io.cout @[Adder4.scala 32:17]
    node s2 = cat(Adder2.io.sum, s1) @[Cat.scala 30:58]
    inst Adder3 of FullAdder_3 @[Adder4.scala 35:22]
    Adder3.clock <= clock
    Adder3.reset <= reset
    node _T_21 = bits(io.A, 3, 3) @[Adder4.scala 36:22]
    Adder3.io.a <= _T_21 @[Adder4.scala 36:15]
    node _T_22 = bits(io.B, 3, 3) @[Adder4.scala 37:22]
    Adder3.io.b <= _T_22 @[Adder4.scala 37:15]
    Adder3.io.cin <= Adder2.io.cout @[Adder4.scala 38:17]
    node _T_23 = cat(Adder3.io.sum, s2) @[Cat.scala 30:58]
    io.Sum <= _T_23 @[Adder4.scala 39:10]
    io.Cout <= Adder3.io.cout @[Adder4.scala 40:11]
    
