circuit Darken : @[:@2.0]
  module Darken : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : UInt<8> @[:@6.4]
    output io_out : UInt<8> @[:@6.4]
  
    node _T_10 = dshl(io_in, UInt<1>("h1")) @[Darken.scala 12:19:@8.4]
    io_out <= bits(_T_10, 7, 0) @[Darken.scala 12:10:@9.4]
