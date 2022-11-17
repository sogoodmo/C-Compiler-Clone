; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
entry:
  %i = alloca i32, align 4
  %PI = alloca float, align 4
  %flag = alloca i1, align 1
  store i1 false, ptr %flag, align 1
  store float 0.000000e+00, ptr %PI, align 4
  store i32 0, ptr %i, align 4
  store i1 true, ptr %flag, align 1
  store float 3.000000e+00, ptr %PI, align 4
  store i32 2, ptr %i, align 4
  br label %loopcond

loopcond:                                         ; preds = %ifcont, %entry
  %i1 = load i32, ptr %i, align 4
  %ilttmp = icmp ult i32 %i1, 100
  %whilecond = select i1 %ilttmp, i1 true, i1 false
  br i1 %whilecond, label %while, label %whilecont

while:                                            ; preds = %loopcond
  %flag2 = load i1, ptr %flag, align 1
  %ifcond = select i1 %flag2, i1 true, i1 false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %while
  %PI3 = load float, ptr %PI, align 4
  %i4 = load i32, ptr %i, align 4
  %i5 = load i32, ptr %i, align 4
  %addtmp = add i32 %i5, 1
  %multmp = mul i32 %i4, %addtmp
  %i6 = load i32, ptr %i, align 4
  %addtmp7 = add i32 %i6, 2
  %multmp8 = mul i32 %multmp, %addtmp7
  %"/" = sitofp i32 %multmp8 to float
  %fdivtmp = fdiv float 4.000000e+00, %"/"
  %faddtmp = fadd float %PI3, %fdivtmp
  store float %faddtmp, ptr %PI, align 4
  br label %ifcont

else:                                             ; preds = %while
  %PI9 = load float, ptr %PI, align 4
  %i10 = load i32, ptr %i, align 4
  %i11 = load i32, ptr %i, align 4
  %addtmp12 = add i32 %i11, 1
  %multmp13 = mul i32 %i10, %addtmp12
  %i14 = load i32, ptr %i, align 4
  %addtmp15 = add i32 %i14, 2
  %multmp16 = mul i32 %multmp13, %addtmp15
  %"/17" = sitofp i32 %multmp16 to float
  %fdivtmp18 = fdiv float 4.000000e+00, %"/17"
  %fsubtmp = fsub float %PI9, %fdivtmp18
  store float %fsubtmp, ptr %PI, align 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %flag19 = load i1, ptr %flag, align 1
  %nottmp = xor i1 %flag19, true
  store i1 %nottmp, ptr %flag, align 1
  %i20 = load i32, ptr %i, align 4
  %addtmp21 = add i32 %i20, 2
  store i32 %addtmp21, ptr %i, align 4
  br label %loopcond

whilecont:                                        ; preds = %loopcond
  %PI22 = load float, ptr %PI, align 4
  ret float %PI22
}
