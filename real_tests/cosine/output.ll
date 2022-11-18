; ModuleID = 'mini-c'
source_filename = "mini-c"

declare float @print_float(float)

define float @cosine(float %x) {
entry:
  %alt = alloca float, align 4
  %eps = alloca float, align 4
  %term = alloca float, align 4
  %n = alloca float, align 4
  %cos = alloca float, align 4
  %x1 = alloca float, align 4
  store float %x, ptr %x1, align 4
  store float 0.000000e+00, ptr %cos, align 4
  store float 0.000000e+00, ptr %n, align 4
  store float 0.000000e+00, ptr %term, align 4
  store float 0.000000e+00, ptr %eps, align 4
  store float 0.000000e+00, ptr %alt, align 4
  store float 0x3EB0C6F7A0000000, ptr %eps, align 4
  store float 1.000000e+00, ptr %n, align 4
  store float 1.000000e+00, ptr %cos, align 4
  store float 1.000000e+00, ptr %term, align 4
  store float -1.000000e+00, ptr %alt, align 4
  br label %loopcond

loopcond:                                         ; preds = %while, %entry
  %term2 = load float, ptr %term, align 4
  %eps3 = load float, ptr %eps, align 4
  %fgttmp = fcmp ogt float %term2, %eps3
  %whilecond = or i1 %fgttmp, false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %term4 = load float, ptr %term, align 4
  %x5 = load float, ptr %x1, align 4
  %fmultmp = fmul float %term4, %x5
  %x6 = load float, ptr %x1, align 4
  %fmultmp7 = fmul float %fmultmp, %x6
  %n8 = load float, ptr %n, align 4
  %fdivtmp = fdiv float %fmultmp7, %n8
  %n9 = load float, ptr %n, align 4
  %faddtmp = fadd float %n9, 1.000000e+00
  %fdivtmp10 = fdiv float %fdivtmp, %faddtmp
  store float %fdivtmp10, ptr %term, align 4
  %cos11 = load float, ptr %cos, align 4
  %alt12 = load float, ptr %alt, align 4
  %term13 = load float, ptr %term, align 4
  %fmultmp14 = fmul float %alt12, %term13
  %faddtmp15 = fadd float %cos11, %fmultmp14
  store float %faddtmp15, ptr %cos, align 4
  %alt16 = load float, ptr %alt, align 4
  %iminustmp = fneg float %alt16
  store float %iminustmp, ptr %alt, align 4
  %n17 = load float, ptr %n, align 4
  %faddtmp18 = fadd float %n17, 2.000000e+00
  store float %faddtmp18, ptr %n, align 4
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %cos19 = load float, ptr %cos, align 4
  %calltmp = call float @print_float(float %cos19)
  %cos20 = load float, ptr %cos, align 4
  ret float %cos20
}
