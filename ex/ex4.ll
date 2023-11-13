; ModuleID = 'ex4.mlkit'
source_filename = "ex4.mlkit"

define i64 @fact_0(i64 %n_1) {
fact_0:
  %primtmp = icmp eq i64 %n_1, 0
  %ifcond = icmp ne i1 %primtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %fact_0
  br label %ifcont

else:                                             ; preds = %fact_0
  %primtmp1 = sub i64 %n_1, 1
  %calltmp = call i64 @fact_0(i64 %primtmp1)
  %primtmp2 = mul i64 %n_1, %calltmp
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %primtmp2, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @fact_0(i64 5)
  ret i64 %calltmp
}
