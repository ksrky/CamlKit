; ModuleID = 'ex4.mlkit'
source_filename = "ex4.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @fact_2(i64 %n_3) {
fact_2:
  %eqtmp = icmp eq i64 %n_3, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %fact_2
  br label %ifcont

else:                                             ; preds = %fact_2
  %subtmp = sub i64 %n_3, 1
  %calltmp = call i64 @fact_2(i64 %subtmp)
  %multmp = mul i64 %n_3, %calltmp
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %multmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @fact_2(i64 5)
  %calltmp1 = call i64 @printi(i64 %calltmp)
  ret i64 %calltmp1
}
