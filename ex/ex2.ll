; ModuleID = 'ex2.mlkit'
source_filename = "ex2.mlkit"

define i32 @func_4(i32 %x_0) {
func_4:
  %calltmp = call i32 @func_3(i32 %x_0)
  ret i32 %calltmp
}

define i32 @func_3(i32 %prog_2) {
func_3:
  ret i32 %prog_2
}

define i32 @main() {
main:
  %calltmp = call i32 @func_4(i32 5)
  ret i32 %calltmp
}
