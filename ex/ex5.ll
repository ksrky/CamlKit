; ModuleID = 'ex5.mlkit'
source_filename = "ex5.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @iseven_7(i64 %n_8) {
iseven_7:
  %eqtmp = icmp eq i64 %n_8, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %iseven_7
  br label %ifcont

else:                                             ; preds = %iseven_7
  %subtmp = sub i64 %n_8, 1
  %calltmp = call i64 @isodd_12(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @isodd_12(i64 %n_13) {
isodd_12:
  %eqtmp = icmp eq i64 %n_13, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %isodd_12
  br label %ifcont

else:                                             ; preds = %isodd_12
  %subtmp = sub i64 %n_13, 1
  %calltmp = call i64 @iseven_7(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 0, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @iseven_7(i64 10)
  %calltmp1 = call i64 @printi(i64 %calltmp)
  ret i64 %calltmp1
}
