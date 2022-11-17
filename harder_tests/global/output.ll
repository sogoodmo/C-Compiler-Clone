; ModuleID = 'mini-c'
source_filename = "mini-c"

@a = common global i32 0

declare i32 @print_int(i32)

define i32 @foo() {
entry:
  %a = load i32, ptr @a, align 4
  %addtmp = add i32 %a, 1
  store i32 %addtmp, ptr @a, align 4
  %a1 = load i32, ptr @a, align 4
  ret i32 %a1
}

define i32 @global() {
entry:
  store i32 5, ptr @a, align 4
  %calltmp = call i32 @foo()
  ret i32 %calltmp
}
