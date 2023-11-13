; ModuleID = 'ex10.mlkit'
source_filename = "ex10.mlkit"

define i64 @f_0(i64 %x_1) {
f_0:
  %primtmp = add i64 %x_1, 2
  ret i64 %primtmp
}

define i64 @main() {
main:
  %calltmp = call i64 @f_0(i64 3)
  ret i64 %calltmp
}
