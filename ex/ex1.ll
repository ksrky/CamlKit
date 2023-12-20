; ModuleID = 'ex1.mlkit'
source_filename = "ex1.mlkit"

define i32 @func_10(i32 %left_2) {
func_10:
  %envtmp = tail call ptr @malloc(i32 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i32))
  %elmptr = getelementptr ptr, ptr %envtmp, i32 0
  store i32 %left_2, ptr %elmptr, align 4
  %envtmp1 = tail call ptr @malloc(i32 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i32))
  %elmptr2 = getelementptr ptr, ptr %envtmp1, i32 0
  store ptr @func_5, ptr %elmptr2, align 8
  %elmptr3 = getelementptr ptr, ptr %envtmp1, i32 1
  store ptr %envtmp, ptr %elmptr3, align 8
  %elmptr4 = getelementptr ptr, ptr %envtmp1, i32 0
  %elmtmp = load ptr, ptr %elmptr4, align 8
  %elmptr5 = getelementptr ptr, ptr %envtmp1, i32 1
  %elmtmp6 = load ptr, ptr %elmptr5, align 8
  %calltmp = call i32 %elmtmp(ptr %elmtmp6, i32 2)
  ret i32 %calltmp
}

define i32 @func_5(ptr %env_6, i32 %right_3) {
func_5:
  %elmptr = getelementptr ptr, ptr %env_6, i32 0
  %elmtmp = load i32, ptr %elmptr, align 4
  %primtmp = add i32 %elmtmp, %right_3
  %calltmp = call i32 @func_4(i32 %primtmp)
  ret i32 %calltmp
}

define i32 @func_4(i32 %prog_0) {
func_4:
  ret i32 %prog_0
}

declare noalias ptr @malloc(i32)

define i32 @main() {
main:
  %calltmp = call i32 @func_10(i32 1)
  ret i32 %calltmp
}
