; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define float @unary(i32 %n, float %m) {
entry:
  %sum = alloca float, align 4
  %result = alloca float, align 4
  %m2 = alloca float, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store float %m, ptr %m2, align 4
  store float 0.000000e+00, ptr %result, align 4
  store float 0.000000e+00, ptr %sum, align 4
  store float 0.000000e+00, ptr %sum, align 4
  %n3 = load i32, ptr %n1, align 4
  %m4 = load float, ptr %m2, align 4
  %"+" = sitofp i32 %n3 to float
  %faddtmp = fadd float %"+", %m4
  store float %faddtmp, ptr %result, align 4
  %result5 = load float, ptr %result, align 4
  %calltmp = call float @print_float(float %result5)
  %sum6 = load float, ptr %sum, align 4
  %result7 = load float, ptr %result, align 4
  %faddtmp8 = fadd float %sum6, %result7
  store float %faddtmp8, ptr %sum, align 4
  %n9 = load i32, ptr %n1, align 4
  %m10 = load float, ptr %m2, align 4
  %iminustmp = fneg float %m10
  %"+11" = sitofp i32 %n9 to float
  %faddtmp12 = fadd float %"+11", %iminustmp
  store float %faddtmp12, ptr %result, align 4
  %result13 = load float, ptr %result, align 4
  %calltmp14 = call float @print_float(float %result13)
  %sum15 = load float, ptr %sum, align 4
  %result16 = load float, ptr %result, align 4
  %faddtmp17 = fadd float %sum15, %result16
  store float %faddtmp17, ptr %sum, align 4
  %n18 = load i32, ptr %n1, align 4
  %m19 = load float, ptr %m2, align 4
  %iminustmp20 = fneg float %m19
  %iminustmp21 = fneg float %iminustmp20
  %"+22" = sitofp i32 %n18 to float
  %faddtmp23 = fadd float %"+22", %iminustmp21
  store float %faddtmp23, ptr %result, align 4
  %result24 = load float, ptr %result, align 4
  %calltmp25 = call float @print_float(float %result24)
  %sum26 = load float, ptr %sum, align 4
  %result27 = load float, ptr %result, align 4
  %faddtmp28 = fadd float %sum26, %result27
  store float %faddtmp28, ptr %sum, align 4
  %n29 = load i32, ptr %n1, align 4
  %fminustmp = sub i32 0, %n29
  %m30 = load float, ptr %m2, align 4
  %iminustmp31 = fneg float %m30
  %"+32" = sitofp i32 %fminustmp to float
  %faddtmp33 = fadd float %"+32", %iminustmp31
  store float %faddtmp33, ptr %result, align 4
  %result34 = load float, ptr %result, align 4
  %calltmp35 = call float @print_float(float %result34)
  %sum36 = load float, ptr %sum, align 4
  %result37 = load float, ptr %result, align 4
  %faddtmp38 = fadd float %sum36, %result37
  store float %faddtmp38, ptr %sum, align 4
  %sum39 = load float, ptr %sum, align 4
  ret float %sum39
}
