; ModuleID = 'mini-c'
source_filename = "mini-c"

declare void @no_arg(i32)

define i32 @test_func() {
entry:
  %x = alloca i32, align 4
  %x1 = load i32, i32 0, align 4
  %addtmp = add i32 1, %x1
  store i32 %addtmp, ptr %x, align 4
  %calltmp = call void @no_arg(i32 3)
}
