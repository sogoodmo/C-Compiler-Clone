; ModuleID = 'mini-c'
source_filename = "mini-c"

@mutable_var = common global i32 0

define i32 @mutating_function() {
entry:
  %mutable_var = load i32, ptr @mutable_var, align 4
  %addtmp = add i32 %mutable_var, 1
  store i32 %addtmp, ptr @mutable_var, align 4
  ret i32 1
}

define i32 @lazyeval_and(i32 %control) {
entry:
  %tmpLazy = alloca i1, align 1
  %control1 = alloca i32, align 4
  store i32 %control, ptr %control1, align 4
  store i32 0, ptr @mutable_var, align 4
  br label %LExpr

LExpr:                                            ; preds = %entry
  %control2 = load i32, ptr %control1, align 4
  %ieqtmp = icmp eq i32 %control2, 1
  br i1 %ieqtmp, label %RExpr, label %ExprCont

RExpr:                                            ; preds = %LExpr
  %calltmp = call i32 @mutating_function()
  %"&&" = icmp ne i32 %calltmp, 0
  store i1 %"&&", ptr %tmpLazy, align 1

ExprCont:                                         ; preds = %LExpr
  store i1 false, ptr %tmpLazy, align 1
  %exprBool = load i1, ptr %tmpLazy, align 1
  %ifcond = select i1 %exprBool, i1 true, i1 false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %ExprCont
  %mutable_var = load i32, ptr @mutable_var, align 4
  ret i32 %mutable_var
  br label %ifcont

else:                                             ; preds = %ExprCont
  %mutable_var3 = load i32, ptr @mutable_var, align 4
  ret i32 %mutable_var3
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i32 0
}

define i32 @lazyeval_or(i32 %control) {
entry:
  %tmpLazy = alloca i1, align 1
  %control1 = alloca i32, align 4
  store i32 %control, ptr %control1, align 4
  store i32 0, ptr @mutable_var, align 4
  br label %LExpr

LExpr:                                            ; preds = %entry
  %control2 = load i32, ptr %control1, align 4
  %ieqtmp = icmp eq i32 %control2, 1
  br i1 %ieqtmp, label %ExprCont, label %RExpr

RExpr:                                            ; preds = %LExpr
  %calltmp = call i32 @mutating_function()
  %"||" = icmp ne i32 %calltmp, 0
  store i1 %"||", ptr %tmpLazy, align 1

ExprCont:                                         ; preds = %LExpr
  store i1 true, ptr %tmpLazy, align 1
  %exprBool = load i1, ptr %tmpLazy, align 1
  %ifcond = select i1 %exprBool, i1 true, i1 false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %ExprCont
  %mutable_var = load i32, ptr @mutable_var, align 4
  ret i32 %mutable_var
  br label %ifcont

else:                                             ; preds = %ExprCont
  %mutable_var3 = load i32, ptr @mutable_var, align 4
  ret i32 %mutable_var3
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i32 0
}
