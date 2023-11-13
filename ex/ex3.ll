; ModuleID = 'ex3.mlkit'
source_filename = "ex3.mlkit"

define i64 @quad_5(i64 %x_6) {
quad_5:
  %calltmp = call i64 @double_7(i64 %x_6)
  %calltmp1 = call i64 @double_7(i64 %calltmp)
  ret i64 %calltmp1
}

define i64 @double_7(i64 %x_8) {
double_7:
  %primtmp = add i64 %x_8, %x_8
  ret i64 %primtmp
}

define i64 @main() {
main:
  %calltmp = call i64 @quad_5(i64 123)
  ret i64 %calltmp
}
