; ModuleID = 'ex3.mlkit'
source_filename = "ex3.mlkit"

define i64 @quad_0(i64 %x_1) {
quad_0:
  %calltmp = call i64 @double_2(i64 %x_1)
  %calltmp1 = call i64 @double_2(i64 %calltmp)
  ret i64 %calltmp1
}

define i64 @double_2(i64 %x_3) {
double_2:
  %primtmp = add i64 %x_3, %x_3
  ret i64 %primtmp
}

define i64 @main() {
main:
  %calltmp = call i64 @quad_0(i64 123)
  ret i64 %calltmp
}
