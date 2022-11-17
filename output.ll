; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @add() {
entry:
  %x = alloca i32, align 4
  store i32 10, ptr %x, align 4
  br label %loopcond

loopcond:                                         ; preds = %while, %entry
  %x1 = load i32, ptr %x, align 4
  %igttmp = icmp ugt i32 %x1, 1
  %whilecond = select i1 %igttmp, i1 true, i1 false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %x2 = load i32, ptr %x, align 4
  %subtmp = sub i32 %x2, 1
  store i32 %subtmp, ptr %x, align 4
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %x3 = load i32, ptr %x, align 4
  ret i32 %x3
}
