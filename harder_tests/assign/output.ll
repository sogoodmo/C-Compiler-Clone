; ModuleID = 'mini-c'
source_filename = "mini-c"

@z = common global i32 0

declare i32 @print_int(i32)

declare float @print_float(float)

define i32 @assign() {
entry:
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 0, ptr %x, align 4
  store i32 0, ptr %y, align 4
  store i32 2, ptr %y, align 4
  store i32 2, ptr %x, align 4
  store i32 3, ptr %y, align 4
  store i32 3, ptr @z, align 4
  %x1 = load i32, ptr %x, align 4
  %y2 = load i32, ptr %y, align 4
  %addtmp = add i32 %x1, %y2
  %z = load i32, ptr @z, align 4
  %addtmp3 = add i32 %addtmp, %z
  ret i32 %addtmp3
}
