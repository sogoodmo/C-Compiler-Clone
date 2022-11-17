; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @add() {
entry:
  %righttest = alloca i32, align 4
  store i32 -4, ptr %righttest, align 4
  %righttest1 = load i32, ptr %righttest, align 4
  ret i32 %righttest1
}
