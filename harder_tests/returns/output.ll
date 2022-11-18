; ModuleID = 'mini-c'
source_filename = "mini-c"

define void @returns(i32 %x) {
entry:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  ret void
}
