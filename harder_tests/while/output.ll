; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define i32 @foo(i32 %x) {
entry:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  br label %loopcond

loopcond:                                         ; preds = %ifcont, %entry
  %x2 = load i32, ptr %x1, align 4
  %ilttmp = icmp slt i32 %x2, 10
  %whilecond = select i1 %ilttmp, i1 true, i1 false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %x3 = load i32, ptr %x1, align 4
  %ilttmp4 = icmp slt i32 %x3, 5
  %ifcond = select i1 %ilttmp4, i1 true, i1 false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %while
  %x5 = load i32, ptr %x1, align 4
  %addtmp = add i32 %x5, 2
  store i32 %addtmp, ptr %x1, align 4
  br label %ifcont

else:                                             ; preds = %while
  %x6 = load i32, ptr %x1, align 4
  %addtmp7 = add i32 %x6, 1
  store i32 %addtmp7, ptr %x1, align 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %x8 = load i32, ptr %x1, align 4
  ret i32 %x8
}
