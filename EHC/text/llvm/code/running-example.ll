%vr0 = inttoptr i64 %p1 to i64*
%vr1 = getelementptr i64* %vr0, i64 1
%vr2 = load i64* %vr1
%vr3 = load i64* %x78
%vr4 = call i64 @primSubInt( i64 %vr3, i64 %vr2 )
store i64 %vr4, i64* %i4
