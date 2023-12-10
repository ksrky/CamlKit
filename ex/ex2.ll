; ModuleID = 'ex2.mlkit'
source_filename = "ex2.mlkit"

define i32 @func_6(i32 %x_0) {
func_6:
  %calltmp = call i32 @func_5(i32 %x_0)
  ret i32 %calltmp
}

define i32 @func_5(i32 %x_2) {
func_5:
  ret i32 %x_2
}

define i32 @main() {
main:
  %calltmp = call i32 @func_6(i32 5)
  ret i32 %calltmp
}
