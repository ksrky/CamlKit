; ModuleID = 'ex4.mlkit'
source_filename = "ex4.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @fact_7(i64 %n_8) {
fact_7:
  %eqtmp = icmp eq i64 %n_8, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %fact_7
  br label %ifcont

else:                                             ; preds = %fact_7
  %subtmp = sub i64 %n_8, 1
  %calltmp = call i64 @fact_7(i64 %subtmp)
  %multmp = mul i64 %n_8, %calltmp
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %multmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @fact_7(i64 5)
  %calltmp1 = call i64 @printi(i64 %calltmp)
  ret i64 %calltmp1
}
