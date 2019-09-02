;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit VecShiftRegisterSimple : 
  module VecShiftRegisterSimple : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<8>, out : UInt<8>}
    
    wire _T_12 : UInt<8>[4] @[VecShiftRegisterSimple.scala 18:31]
    _T_12[0] <= UInt<8>("h00") @[VecShiftRegisterSimple.scala 18:31]
    _T_12[1] <= UInt<8>("h00") @[VecShiftRegisterSimple.scala 18:31]
    _T_12[2] <= UInt<8>("h00") @[VecShiftRegisterSimple.scala 18:31]
    _T_12[3] <= UInt<8>("h00") @[VecShiftRegisterSimple.scala 18:31]
    reg delays : UInt<8>[4], clock with : (reset => (reset, _T_12)) @[VecShiftRegisterSimple.scala 18:23]
    delays[0] <= io.in @[VecShiftRegisterSimple.scala 20:13]
    delays[1] <= delays[0] @[VecShiftRegisterSimple.scala 21:13]
    delays[2] <= delays[1] @[VecShiftRegisterSimple.scala 22:13]
    delays[3] <= delays[2] @[VecShiftRegisterSimple.scala 23:13]
    io.out <= delays[3] @[VecShiftRegisterSimple.scala 24:13]
    
