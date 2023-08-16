; ModuleID = 'main'
source_filename = "main"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @iseven_2(i64 %n_3) {
iseven_2:
  %eqtmp = icmp eq i64 %n_3, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %iseven_2
  br label %ifcont

else:                                             ; preds = %iseven_2
  %subtmp = sub i64 %n_3, 1
  %calltmp = call i64 @isodd_7(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @isodd_7(i64 %n_8) {
isodd_7:
  %eqtmp = icmp eq i64 %n_8, 0
  %booltmp = sext i1 %eqtmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %isodd_7
  br label %ifcont

else:                                             ; preds = %isodd_7
  %subtmp = sub i64 %n_8, 1
  %calltmp = call i64 @iseven_2(i64 %subtmp)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 0, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @iseven_2(i64 10)
  %calltmp1 = call i64 @printi(i64 %calltmp)
  ret i64 %calltmp1
}
