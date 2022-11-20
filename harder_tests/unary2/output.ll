; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @unary2() {
entry:
  %tmpLazy = alloca i1, align 1
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
  br i1 true, label %SkipRExpr, label %RExpr5

RExpr5:                                           ; preds = %entry
  %b6 = load i1, ptr %b, align 1
  store i1 %b6, ptr %tmpLazy, align 1
  br label %Cont

SkipRExpr:                                        ; preds = %entry
  store i1 true, ptr %tmpLazy, align 1
  br label %Cont

Cont:                                             ; preds = %SkipRExpr, %RExpr5
  %exprBool = load i1, ptr %tmpLazy, align 1
  store i1 %exprBool, ptr %b2, align 1
  %b27 = load i1, ptr %b2, align 1
  %i28 = zext i1 %b27 to i32
  store i32 %i28, ptr %i2, align 4
  %i29 = load i32, ptr %i2, align 4
  ret i32 %i29
}
