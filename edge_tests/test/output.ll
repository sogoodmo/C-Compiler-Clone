; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @test() {
entry:
  br i1 false, label %then, label %ifcont2

then:                                             ; preds = %entry
  br i1 true, label %then1, label %else

then1:                                            ; preds = %then
  ret i32 3
  br label %ifcont

else:                                             ; preds = %then
  ret i32 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then1
  br label %ifcont2

ifcont2:                                          ; preds = %ifcont, %entry
  ret i32 0
}
