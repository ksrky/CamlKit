; ModuleID = 'ex7.mlkit'
source_filename = "ex7.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @ackermann_7(i64 %m_8, i64 %n_9) {
ackermann_7:
  %eqtmp = icmp eq i64 %m_8, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %ackermann_7
  %addtmp = add i64 %n_9, 1
  br label %ifcont10

else:                                             ; preds = %ackermann_7
  %eqtmp1 = icmp eq i64 %n_9, 0
  %booltmp2 = sext i1 %eqtmp1 to i64
  %ifcond3 = icmp ne i64 %booltmp2, 0
  br i1 %ifcond3, label %then4, label %else5

then4:                                            ; preds = %else
  %subtmp = sub i64 %m_8, 1
  %calltmp = call i64 @ackermann_7(i64 %subtmp, i64 1)
  br label %ifcont

else5:                                            ; preds = %else
  %subtmp6 = sub i64 %m_8, 1
  %subtmp7 = sub i64 %n_9, 1
  %calltmp8 = call i64 @ackermann_7(i64 %m_8, i64 %subtmp7)
  %calltmp9 = call i64 @ackermann_7(i64 %subtmp6, i64 %calltmp8)
  br label %ifcont

ifcont:                                           ; preds = %else5, %then4
  %iftmp = phi i64 [ %calltmp, %then4 ], [ %calltmp9, %else5 ]
  br label %ifcont10

ifcont10:                                         ; preds = %ifcont, %then
  %iftmp11 = phi i64 [ %addtmp, %then ], [ %iftmp, %ifcont ]
  ret i64 %iftmp11
}

define i64 @main() {
main:
  %calltmp = call i64 @ackermann_7(i64 4, i64 1)
  ret i64 %calltmp
}
