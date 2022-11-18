; ModuleID = 'mini-c'
source_filename = "mini-c"

@test = common global i32 0
@f = common global float 0.000000e+00
@b = common global i1 false

declare i32 @print_int(i32)

define i32 @While(i32 %n) {
entry:
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %result, align 4
  store i32 12, ptr @test, align 4
  store i32 0, ptr %result, align 4
  %test = load i32, ptr @test, align 4
  %calltmp = call i32 @print_int(i32 %test)
  br label %loopcond

loopcond:                                         ; preds = %while, %entry
  %result2 = load i32, ptr %result, align 4
  %ilttmp = icmp slt i32 %result2, 10
  %whilecond = or i1 %ilttmp, false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %result3 = load i32, ptr %result, align 4
  %addtmp = add i32 %result3, 1
  store i32 %addtmp, ptr %result, align 4
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %result4 = load i32, ptr %result, align 4
  ret i32 %result4
}
