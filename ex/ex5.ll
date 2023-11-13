; ModuleID = 'ex5.mlkit'
source_filename = "ex5.mlkit"

define i64 @isodd_5(i64 %n_6) {
isodd_5:
  %primtmp = icmp eq i64 %n_6, 0
  %ifcond = icmp ne i1 %primtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %isodd_5
  br label %ifcont

else:                                             ; preds = %isodd_5
  %primtmp1 = sub i64 %n_6, 1
  %calltmp = call i64 @iseven_0(i64 %primtmp1)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 0, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @iseven_0(i64 %n_1) {
iseven_0:
  %primtmp = icmp eq i64 %n_1, 0
  %ifcond = icmp ne i1 %primtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %iseven_0
  br label %ifcont

else:                                             ; preds = %iseven_0
  %primtmp1 = sub i64 %n_1, 1
  %calltmp = call i64 @isodd_5(i64 %primtmp1)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i64 [ 1, %then ], [ %calltmp, %else ]
  ret i64 %iftmp
}

define i64 @main() {
main:
  %calltmp = call i64 @iseven_0(i64 10)
  ret i64 %calltmp
}
