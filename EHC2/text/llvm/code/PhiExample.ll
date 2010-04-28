define i32 @abs( i32 %x )
{
  %cond = icmp slt i32 %x, 0
  br i1 %cond, label %true, label %false

  true:
    %tmp = mul i32 %x, -1
    br label %return

  false:
    br label %return

  return:
    %res = phi i32 [ %tmp, %true ], [ %x, %false ]
    ret i32 %res
}
