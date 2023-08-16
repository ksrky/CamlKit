; ModuleID = 'main'
source_filename = "main"

declare i64 @printi(i64)

declare i64 @readi(i64)

define i64 @double_4(i64 %x_5) {
double_4:
  %addtmp = add i64 %x_5, %x_5
  ret i64 %addtmp
}

define i64 @main() {
main:
  %calltmp = call i64 @double_4(i64 123)
  %calltmp1 = call i64 @double_4(i64 %calltmp)
  %calltmp2 = call i64 @printi(i64 %calltmp1)
  ret i64 %calltmp2
}
