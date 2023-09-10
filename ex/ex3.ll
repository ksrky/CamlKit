; ModuleID = 'ex3.mlkit'
source_filename = "ex3.mlkit"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @double_7(i64 %x_8) {
double_7:
  %addtmp = add i64 %x_8, %x_8
  ret i64 %addtmp
}

define i64 @main() {
main:
  %calltmp = call i64 @double_7(i64 123)
  %calltmp1 = call i64 @double_7(i64 %calltmp)
  %calltmp2 = call i64 @printi(i64 %calltmp1)
  %calltmp3 = call i64 @a_14(i64 %calltmp2)
  ret i64 %calltmp3
}
