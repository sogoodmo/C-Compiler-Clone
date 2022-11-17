; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @unary2() {
entry:
  %z = alloca i32, align 4
  %b2 = alloca i1, align 1
  %b = alloca i1, align 1
  %i2 = alloca i32, align 4
  %i = alloca i32, align 4
  %f = alloca float, align 4
  store float 0.000000e+00, ptr %f, align 4
  store i32 0, ptr %i, align 4
  store i32 0, ptr %i2, align 4
  store i1 false, ptr %b, align 1
  store i1 false, ptr %b2, align 1
  store i32 0, ptr %z, align 4
  store float 0.000000e+00, ptr %f, align 4
  store i32 1, ptr %i, align 4
  %f1 = load float, ptr %f, align 4
  %"!" = fcmp one float %f1, 0.000000e+00
  %finottmp = xor i1 %"!", true
  %bminustmp = sub i1 false, %finottmp
  %i3 = load i32, ptr %i, align 4
  %"+" = zext i1 %bminustmp to i32
  %addtmp = add i32 %"+", %i3
  %b4 = icmp ne i32 %addtmp, 0
  store i1 %b4, ptr %b, align 1
  %b5 = load i1, ptr %b, align 1
  %ortmp = or i1 true, %b5
  store i1 %ortmp, ptr %b2, align 1
  %b26 = load i1, ptr %b2, align 1
  %i27 = zext i1 %b26 to i32
  store i32 %i27, ptr %i2, align 4
  %i28 = load i32, ptr %i2, align 4
  ret i32 %i28
}
