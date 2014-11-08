; Begin of the standard declarations and definitions every Monty program needs.
declare void @exit(i32 %status) noreturn
declare i8* @malloc(i32 %size) nounwind
declare i32 @printf(i8* %format, ... ) nounwind

@.stringFormat = private constant [3 x i8] c"%s\00";
@.floatFormat = private constant [3 x i8] c"%g\00";
@.intFormat = private constant [3 x i8] c"%i\00";
@.charFormat = private constant [3 x i8] c"%c\00";
@.lineStringFormat = private constant [4 x i8] c"%s\0a\00";
@.lineFloatFormat = private constant [4 x i8] c"%g\0a\00";
@.lineIntFormat = private constant [4 x i8] c"%i\0a\00";
@.lineCharFormat = private constant [4 x i8] c"%c\0a\00";

; Search the sourceCTData array of pointers to vmtData for the toVMTPtr
; pointer for class inheritance-test.
define i1 @vmt_isa_class([0 x i8*]* %sourceCTData, i8* %toVMTPtr) {
    %cnt = alloca i32
    store i32 0, i32* %cnt
    br label %loop.start

    loop.start:
        %cnt_val = load i32* %cnt
        %index = getelementptr [0 x i8*]* %sourceCTData, i32 0, i32 %cnt_val
        %ptr = load i8** %index
        ; If the end of the array is reached (null terminated) fail..
        %cmp_null = icmp eq i8* %ptr, null
        br i1 %cmp_null, label %loop.failure, label %loop.next

    loop.next:
        %cnt_inr = add i32 %cnt_val, 1
        store i32 %cnt_inr, i32* %cnt
        ; If the pointers are equal the class equals or inherits.
        %cmp = icmp eq i8* %toVMTPtr, %ptr
        br i1 %cmp, label %loop.success, label %loop.start

    loop.failure:
        ret i1 0
    loop.success:
        ret i1 1
}

; Check if the given index is within bounds (0 <= index < array.size). If not exit(3)
define void @array_bounds_check({ i64, [0 x i8*] }* %array, i64 %index) {
    %gt_zero = icmp sge i64 %index, 0
    br i1 %gt_zero, label %bounds.next, label %bounds.error

    bounds.next:
        %size_field = getelementptr inbounds {i64, [0 x i8*]}* %array, i32 0, i32 0
        %size = load i64* %size_field
        %lt_size = icmp slt i64 %index, %size
        br i1 %lt_size, label %bounds.success, label %bounds.error

    bounds.error:
        call void @exit(i32 3)
        ret void
    bounds.success:
        ret void
}
; End of the standard declarations and definitions every Monty program needs.

