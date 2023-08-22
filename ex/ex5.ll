; ModuleID = 'ex5.mlkit'
source_filename = "ex5.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @iseven_5(i64 %n_6) {
iseven_5:
  %eqtmp = icmp eq i64 %n_6, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %iseven_5
  br label %ifcont

else:                                             ; preds = %iseven_5
  %subtmp = sub i64 %n_6, 1
  %calltmp = call i64 @isodd_10(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @isodd_10(i64 %n_11) {
isodd_10:
  %eqtmp = icmp eq i64 %n_11, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %isodd_10
  br label %ifcont

else:                                             ; preds = %isodd_10
  %subtmp = sub i64 %n_11, 1
  %calltmp = call i64 @iseven_5(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 0, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @iseven_5(i64 10)
  %calltmp1 = call i64 @printi(i64 %calltmp)
  ret i64 %calltmp1
}
