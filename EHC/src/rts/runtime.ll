target pointersize = 32
deplibs = [ "c" ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
declare int %printf(sbyte*, ...)
declare void %exit(int)

declare void %fun_main()
declare void %initialize()

%global_False = external global %thunk_type
%global_True  = external global %thunk_type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typedefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%thunk_type = type uint

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%Heap = global %thunk_type* undef
%Stack = global %thunk_type* undef
%ReturnArea = global %thunk_type* undef
%HeapLimit = global %thunk_type* undef

%HP = global %thunk_type* undef
%SP = global %thunk_type* undef
%RP = global %thunk_type* undef
%BP = global %thunk_type* undef

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%__heapErrorMsg   = internal constant [15 x sbyte] c"heap overflow\0A\00"
%__switchErrorMsg = internal constant [23 x sbyte] c"pattern match failure\0A\00"
%__resultMsg      = internal constant [24 x sbyte] c"result tag=%d value=%d\0A\00"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
void %main( ) {

  ; Allocate the heap and save the pointer in %Heap and %HP
  %heap_ptr  = malloc %thunk_type, uint 100000
  store %thunk_type* %heap_ptr, %thunk_type** %Heap
  store %thunk_type* %heap_ptr, %thunk_type** %HP

  ; Get the last element of the allocated heap and save that in %HeapLimit
  %heap_limit_ptr = getelementptr %thunk_type* %heap_ptr, uint 99999
  store %thunk_type* %heap_limit_ptr, %thunk_type** %HeapLimit

  ; Allocate the stack
  %stack_ptr = malloc %thunk_type, uint 100000
  store %thunk_type* %stack_ptr, %thunk_type** %Stack
  store %thunk_type* %stack_ptr, %thunk_type** %SP  

  ; Allocate the returnpointer
  %ret_ptr   = malloc %thunk_type, uint 100    
  store %thunk_type* %ret_ptr, %thunk_type** %ReturnArea
  store %thunk_type* %ret_ptr, %thunk_type** %RP

  tail call void %initialize()
  tail call void %fun_main()

  ; Print RP[0] and RP[1]
  ;
  %rp_0_ptr = load %thunk_type** %RP
  %rp_1_ptr = getelementptr %thunk_type* %rp_0_ptr, uint 1
  %rp_0     = load %thunk_type* %rp_0_ptr
  %rp_1     = load %thunk_type* %rp_1_ptr
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([24 x sbyte]* %__resultMsg, int 0, int 0)
                                      , %thunk_type %rp_0, %thunk_type %rp_1 )
  ret void
}

void %heap_overflow_error() {
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([15 x sbyte]* %__heapErrorMsg, int 0, int 0) )
  tail call void %exit( int 1 )
  unreachable
}

void %switch_trap() {
  tail call int (sbyte*, ...)* %printf( sbyte* getelementptr ([23 x sbyte]* %__switchErrorMsg, int 0, int 0) )
  tail call void %exit( int 1 )
  unreachable
}

uint %primGtInt( uint %l, uint %r ) {

  %cond = setgt uint %l, %r
  br bool %cond, label %gt_True, label %gt_False

  gt_True:
    %tr.tmp.0 = load %thunk_type* %global_True
    %tr.tmp.1 = cast %thunk_type %tr.tmp.0 to %thunk_type*
    %tr.tmp.2 = load %thunk_type* %tr.tmp.1
    ret uint %tr.tmp.2

  gt_False:
    %fs.tmp.0 = load %thunk_type* %global_False
    %fs.tmp.1 = cast %thunk_type %fs.tmp.0 to %thunk_type*
    %fs.tmp.2 = load %thunk_type* %fs.tmp.1
    ret uint %fs.tmp.2
}

uint %primAddInt( uint %l, uint %r ) {

  %tmp.0 = add uint %l, %r
  ret uint %tmp.0
}

%thunk_type* %heapalloc( uint %words ) {

  ; Offset of current pointer is 0 based
  %word_offset = sub uint %words, 1 

  ; The thunk can be allocated on this address
  %curr_ptr = load %thunk_type** %HP
  %new_ptr  = getelementptr %thunk_type* %curr_ptr, uint %word_offset
  %heap_limit = load %thunk_type** %HeapLimit

  ; Check if the new heap pointer is below the heap limit
  %heap_limit_cond = setge %thunk_type* %new_ptr, %heap_limit
  br bool %heap_limit_cond, label %heap_overflow, label %no_heap_overflow

  heap_overflow:
  tail call void %heap_overflow_error()
  ret %thunk_type* %curr_ptr ; Declared unreachable by the heap_overflow_error

  no_heap_overflow:
  store %thunk_type* %new_ptr, %thunk_type** %HP
  ret %thunk_type* %curr_ptr
}
