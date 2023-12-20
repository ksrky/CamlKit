; ModuleID = 'ex0.mlkit'
source_filename = "ex0.mlkit"

define i32 @func_1(i32 %prog_0) {
func_1:
  ret i32 %prog_0
}

define i32 @main() {
main:
  %calltmp = call i32 @func_1(i32 42)
  ret i32 %calltmp
}
