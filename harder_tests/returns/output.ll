; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @returns(i32 %x) {
entry:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  %Igttmp = icmp sgt i32 %x2, 1
  %ifcond = or i1 %Igttmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  ret i32 1
  br label %ifcont

else:                                             ; preds = %entry
  ret i32 2
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i32 3
}
