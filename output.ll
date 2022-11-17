; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @add(i32 %x) {
entry:
  %y = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  store i32 0, ptr %y, align 4
  store i32 -1, ptr %y, align 4
  ret i1 false
}
