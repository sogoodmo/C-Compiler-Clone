; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @returns(i32 %x) {
entry:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  br label %loopcond

loopcond:                                         ; preds = %while, %entry
  %x2 = load i32, ptr %x1, align 4
  %ieqtmp = icmp eq i32 %x2, 1
  %whilecond = select i1 %ieqtmp, i1 true, i1 false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  ret i32 0
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %x3 = load i32, ptr %x1, align 4
  %Igttmp = icmp sgt i32 %x3, 1
  %ifcond = select i1 %Igttmp, i1 true, i1 false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %whilecont
  ret i32 1
  br label %ifcont

else:                                             ; preds = %whilecont
  ret i32 2
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i32 3
}
