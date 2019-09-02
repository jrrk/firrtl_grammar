;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit MaxN : 
  module MaxN : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip ins : UInt<16>[8], out : UInt<16>}
    
    node _T_33 = gt(io.ins[0], io.ins[1]) @[MaxN.scala 13:46]
    node _T_34 = mux(_T_33, io.ins[0], io.ins[1]) @[MaxN.scala 13:43]
    node _T_35 = gt(_T_34, io.ins[2]) @[MaxN.scala 13:46]
    node _T_36 = mux(_T_35, _T_34, io.ins[2]) @[MaxN.scala 13:43]
    node _T_37 = gt(_T_36, io.ins[3]) @[MaxN.scala 13:46]
    node _T_38 = mux(_T_37, _T_36, io.ins[3]) @[MaxN.scala 13:43]
    node _T_39 = gt(_T_38, io.ins[4]) @[MaxN.scala 13:46]
    node _T_40 = mux(_T_39, _T_38, io.ins[4]) @[MaxN.scala 13:43]
    node _T_41 = gt(_T_40, io.ins[5]) @[MaxN.scala 13:46]
    node _T_42 = mux(_T_41, _T_40, io.ins[5]) @[MaxN.scala 13:43]
    node _T_43 = gt(_T_42, io.ins[6]) @[MaxN.scala 13:46]
    node _T_44 = mux(_T_43, _T_42, io.ins[6]) @[MaxN.scala 13:43]
    node _T_45 = gt(_T_44, io.ins[7]) @[MaxN.scala 13:46]
    node _T_46 = mux(_T_45, _T_44, io.ins[7]) @[MaxN.scala 13:43]
    io.out <= _T_46 @[MaxN.scala 19:10]
    
