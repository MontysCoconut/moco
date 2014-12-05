; Begin of the standard declarations and definitions every Monty program needs.
declare void @exit(i32 %status) noreturn
declare i8* @malloc(i64 %size) nounwind
declare i8* @realloc(i8* %ptr, i64 %size) nounwind
declare void @free(i8* %ptr) nounwind

declare %struct._IO_FILE* @fdopen(i64, i8*) nounwind
declare i32 @fgetc(%struct._IO_FILE*) nounwind
declare i8* @fgets(i8*, i64, %struct._IO_FILE*) nounwind
declare i32 @printf(i8* %format, ... ) nounwind

@.stringFormat = private constant [3 x i8] c"%s\00";
@.floatFormat = private constant [3 x i8] c"%g\00";
@.intFormat = private constant [3 x i8] c"%i\00";
@.charFormat = private constant [3 x i8] c"%c\00";
@.lineStringFormat = private constant [4 x i8] c"%s\0a\00";
@.lineFloatFormat = private constant [4 x i8] c"%g\0a\00";
@.lineIntFormat = private constant [4 x i8] c"%i\0a\00";
@.lineCharFormat = private constant [4 x i8] c"%c\0a\00";

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*,
                          i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*,
                          i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*,
                          i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
@.stdin_mode = private constant [2 x i8] c"r\00"

; Read at most <num> bytes from stdin and return the string if success. exit(4) in case of failure.
define i8* @read_helper(i64 %num) {
    %nump = add i64 %num, 1
    %str = call i8* @malloc(i64 %nump)
    %stdin = call %struct._IO_FILE* @fdopen(i64 0, i8* getelementptr inbounds ([2 x i8]* @.stdin_mode, i32 0, i32 0))
    %res = call i8* @fgets(i8* %str, i64 %nump, %struct._IO_FILE* %stdin)
    %cmp_null = icmp eq i8* %res, null
    br i1 %cmp_null, label %fgets.error, label %fgets.success

    fgets.error:
        call void @free(i8* %str)
        call void @exit(i32 4)
        ret i8* null;

    fgets.success:
        ret i8* %str
}

; Read a \n terminated string from stdin and return if success. exit(4) in
; case of failure.
define i8* @readln_helper() {
    %line = alloca i8*
    %linep = alloca i8*
    %lenmax = alloca i64
    %len = alloca i64
    %c = alloca i32
    %linen = alloca i8*

    %stdin = call %struct._IO_FILE* @fdopen(i64 0, i8* getelementptr inbounds ([2 x i8]* @.stdin_mode, i32 0, i32 0))

    %malloc_ptr = call i8* @malloc(i64 100)
    store i8* %malloc_ptr, i8** %line
    store i8* %malloc_ptr, i8** %linep
    store i64 100, i64* %lenmax
    store i64 100, i64* %len
    %malloc_is_null = icmp eq i8* %malloc_ptr, null
    br i1 %malloc_is_null, label %readln.error, label %readln.lp.start

    readln.lp.start:
        %char = call i32 @fgetc(%struct._IO_FILE* %stdin)
        store i32 %char, i32* %c
        %char_is_eof = icmp eq i32 %char, -1
        br i1 %char_is_eof, label %readln.success, label %readln.lp.no_eof

    readln.lp.no_eof:
        %len_raw = load i64* %len
        %len_minus = add i64 %len_raw, -1
        store i64 %len_minus, i64* %len
        %len_is_zero = icmp eq i64 %len_minus, 0
        br i1 %len_is_zero, label %readln.lp.realloc, label %readln.lp.search_newline

    readln.lp.realloc:
        %lenmax_raw = load i64* %lenmax
        store i64 %lenmax_raw, i64* %len
        %new_size = mul i64 %lenmax_raw, 2
        store i64 %new_size, i64* %lenmax
        %linep_raw = load i8** %linep
        %new_linep = call i8* @realloc(i8* %linep_raw, i64 %new_size)
        store i8* %new_linep, i8** %linen
        %is_null = icmp eq i8* %new_linep, null
        br i1 %is_null, label %readln.error, label %readln.lp.realloc.success

    readln.lp.realloc.success:
        %linen_raw = load i8** %linen
        %line_raw = load i8** %line
        %linep_raw_ = load i8** %linep
        %line_int = ptrtoint i8* %line_raw to i64
        %linep_int = ptrtoint i8* %linep_raw_ to i64
        %substract = sub i64 %line_int, %linep_int
        %new_line = getelementptr inbounds i8* %linen_raw, i64 %substract
        store i8* %new_line, i8** %line
        store i8* %linen_raw, i8** %linep
        br label %readln.lp.search_newline

    readln.lp.search_newline:
        %char_ = load i32* %c
        %chart = trunc i32 %char_ to i8
        %raw_line = load i8** %line
        %new_line_ = getelementptr inbounds i8* %raw_line, i32 1
        store i8* %new_line_, i8** %line
        store i8 %chart, i8* %raw_line
        %is_newline = icmp eq i32 %char_, 10
        br i1 %is_newline, label %readln.success, label %readln.lp.start

    readln.error:
        %to_free = load i8** %linep
        call void @free(i8* %to_free)
        call void @exit(i32 4)
        ret i8* null;

    readln.success:
        %zero_extend = load i8** %line
        store i8 0, i8* %zero_extend
        %ret = load i8** %linep
        ret i8* %ret
}

; Search the sourceCTData array of pointers to vmtData for the toVMTPtr
; pointer for class inheritance-test.
define i1 @vmt_isa_class([0 x i8*]* %sourceCTData, i8* %toVMTPtr) {
    %cnt = alloca i64
    store i64 0, i64* %cnt
    br label %loop.start

    loop.start:
        %cnt_val = load i64* %cnt
        %index = getelementptr [0 x i8*]* %sourceCTData, i32 0, i64 %cnt_val
        %ptr = load i8** %index
        ; If the end of the array is reached (null terminated) fail..
        %cmp_null = icmp eq i8* %ptr, null
        br i1 %cmp_null, label %loop.failure, label %loop.next

    loop.next:
        %cnt_inr = add i64 %cnt_val, 1
        store i64 %cnt_inr, i64* %cnt
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

