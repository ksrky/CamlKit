; ModuleID = 'ex2.mlkit'
source_filename = "ex2.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @main() {
main:
  %calltmp = call i64 @printi(i64 5)
  ret i64 %calltmp
}