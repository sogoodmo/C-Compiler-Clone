; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n) {
entry:
  %total = alloca i32, align 4
  %c = alloca i32, align 4
  %next = alloca i32, align 4
  %second = alloca i32, align 4
  %first = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %first, align 4
  store i32 0, ptr %second, align 4
  store i32 0, ptr %next, align 4
  store i32 0, ptr %c, align 4
  store i32 0, ptr %total, align 4
  %n2 = load i32, ptr %n1, align 4
  %calltmp = call i32 @print_int(i32 %n2)
  store i32 0, ptr %first, align 4
  store i32 1, ptr %second, align 4
  store i32 1, ptr %c, align 4
  store i32 0, ptr %total, align 4
  br label %loopcond

loopcond:                                         ; preds = %ifcont, %entry
  %c3 = load i32, ptr %c, align 4
  %n4 = load i32, ptr %n1, align 4
  %ilttmp = icmp slt i32 %c3, %n4
  %whilecond = or i1 %ilttmp, false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %c5 = load i32, ptr %c, align 4
  %ilteqtmp = icmp sle i32 %c5, 1
  %ifcond = or i1 %ilteqtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %while
  %c6 = load i32, ptr %c, align 4
  store i32 %c6, ptr %next, align 4
  br label %ifcont

else:                                             ; preds = %while
  %first7 = load i32, ptr %first, align 4
  %second8 = load i32, ptr %second, align 4
  %addtmp = add i32 %first7, %second8
  store i32 %addtmp, ptr %next, align 4
  %second9 = load i32, ptr %second, align 4
  store i32 %second9, ptr %first, align 4
  %next10 = load i32, ptr %next, align 4
  store i32 %next10, ptr %second, align 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %next11 = load i32, ptr %next, align 4
  %calltmp12 = call i32 @print_int(i32 %next11)
  %c13 = load i32, ptr %c, align 4
  %addtmp14 = add i32 %c13, 1
  store i32 %addtmp14, ptr %c, align 4
  %total15 = load i32, ptr %total, align 4
  %next16 = load i32, ptr %next, align 4
  %addtmp17 = add i32 %total15, %next16
  store i32 %addtmp17, ptr %total, align 4
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %total18 = load i32, ptr %total, align 4
  %calltmp19 = call i32 @print_int(i32 %total18)
  %total20 = load i32, ptr %total, align 4
  ret i32 %total20
}
