; ModuleID = 'ex0.mlkit'
source_filename = "ex0.mlkit"

define i64 @lamtmp_2(i64 %x_0) {
lamtmp_2:
  ret i64 %x_0
}

define i64 @main() {
main:
  %calltmp = call i64 @lamtmp_2(i64 42)
  ret i64 %calltmp
}
