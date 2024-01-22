; ModuleID = 'ex3.mlkit'
source_filename = "ex3.mlkit"

define i32 @func_15(i32 %x_1) {
func_15:
  %calltmp = call i32 @func_14(i32 %x_1)
  %calltmp1 = call i32 @func_14(i32 %calltmp)
  ret i32 %calltmp1
}

define i32 @func_14(i32 %x_3) {
func_14:
  %primtmp = add i32 %x_3, %x_3
  ret i32 %primtmp
}

define i32 @main() {
main:
  %calltmp = call i32 @func_15(i32 12)
  ret i32 %calltmp
}
