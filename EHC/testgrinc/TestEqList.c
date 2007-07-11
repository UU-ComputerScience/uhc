// Generated C code
#include "rts.h"

// Tag constants
#define UNBOXED 1
#define CFalse 0
#define CTrue 1
#define CDictminusEq 0
#define Ccolon 0
#define Csubbus 1
#define HOLE 5
#define P2_fun_10_0_46_0 6
#define P2_fun_10_0_54_0 7
#define P2_fun_10_0_85_0 8
#define P2_fun_primEqInt 9
#define Ffun_slasheq 10
#define Ffun_eqeq 11
#define Ffun__res_0_11_0__43_0tilde25 12
#define Ffun__res_0_9_0__59_0tilde40 13
#define Ffun_0_9_0 14
#define Ffun_10_0_68 15
#define Ffun_10_0_70 16
#define Ffun_1_390_2_0 17
#define Ffun_main 18
#define Aapp2 19

// Global table
GrWord global_subbus;
GrWord global_False;
GrWord global_True;
GrWord global_1_390_2_0;
GrWord global_10_0_70;
GrWord global_10_0_68;
GrWord global_main;
GrWord global_0_9_0;

// Auxiliary variable
GrWord auxVar;
GrWord auxPtr;

// Function definitions
int silly_main()
{
    *--SP = (GrWord)(&&mainlab1);
    *--SP = (GrWord)0;
    goto initialize;
    mainlab1:
    *--SP = (GrWord)(&&mainlab2);
    *--SP = (GrWord)0;
    goto fun_main;
    mainlab2:
    if (RP[1]==0) { RP[0] = SP[-1]; RP[1] = SP[-2]; }
    return 0;

initialize:
    SP = SP+-2;
    global_subbus = heapalloc(2);
    ((Pointer)global_subbus)[0] = Csubbus;
    global_False = heapalloc(2);
    ((Pointer)global_False)[0] = CFalse;
    global_True = heapalloc(2);
    ((Pointer)global_True)[0] = CTrue;
    global_1_390_2_0 = heapalloc(3);
    ((Pointer)global_1_390_2_0)[0] = Ffun_1_390_2_0;
    global_10_0_70 = heapalloc(3);
    ((Pointer)global_10_0_70)[0] = Ffun_10_0_70;
    global_10_0_68 = heapalloc(3);
    ((Pointer)global_10_0_68)[0] = Ffun_10_0_68;
    global_main = heapalloc(2);
    ((Pointer)global_main)[0] = Ffun_main;
    global_0_9_0 = heapalloc(3);
    ((Pointer)global_0_9_0)[0] = Ffun_0_9_0;
    SP = SP+4;
    goto *(SP[-1]);

app2:
    
    SP = SP+-5;
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[7]))[0]) {
        case Ffun_slasheq:
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[7]))[1];
            SP[-2] = ((GrWord)(&&retlab3));
            SP[-3] = ((GrWord)(SP+5));
            SP = SP+-3;
            goto fun_slasheq;
            retlab3:
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            SP[4] = ((Pointer)(SP[7]))[0] = SP[-1];
            SP[3] = ((Pointer)(SP[7]))[1] = SP[-2];
            break;
        case Ffun_eqeq:
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[7]))[1];
            SP[-2] = ((GrWord)(&&retlab4));
            SP[-3] = ((GrWord)(SP+5));
            SP = SP+-3;
            goto fun_eqeq;
            retlab4:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            SP[4] = ((Pointer)(SP[7]))[0] = SP[-1];
            SP[3] = ((Pointer)(SP[7]))[1] = SP[-2];
            SP[2] = ((Pointer)(SP[7]))[2] = SP[-3];
            break;
        case P2_fun_primEqInt:
            // Unit (Internal)
            SP[4] = P2_fun_primEqInt;
            break;
        case P2_fun_10_0_46_0:
            // Fetch
            //  (inlined)
            // Unit (Internal)
            SP[4] = P2_fun_10_0_46_0;
            SP[3] = ((Pointer)(SP[7]))[1];
            break;
        case P2_fun_10_0_85_0:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Unit (Internal)
            SP[4] = P2_fun_10_0_85_0;
            SP[3] = ((Pointer)(SP[7]))[1];
            SP[2] = ((Pointer)(SP[7]))[2];
            break;
    }
    // Case
    switch (SP[4]) {
        case P2_fun_primEqInt:
            // Call (Tail) 
            SP[7] = SP[6];
            SP[6] = SP[5];
            SP = SP+6;
            goto fun_primEqInt;
            break;
        case P2_fun_10_0_46_0:
            // Call (Tail) 
            SP[7] = SP[3];
            SP = SP+5;
            goto fun_10_0_46_0;
            break;
        case P2_fun_10_0_85_0:
            // Call (Tail) 
            SP[7] = SP[2];
            SP[4] = SP[5];
            SP[5] = SP[6];
            SP[6] = SP[3];
            SP = SP+4;
            goto fun_10_0_85_0;
            break;
    }

fun_primEqInt:
    SP = SP+-2;
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // FFI (Yielding)
    SP[5] = (( ((int)( SP[4] )>>1 ) == (int)(( SP[5] )>>1 ) )? CTrue : CFalse );
    SP = SP+6;
    goto *(SP[-3]);

fun_colon:
    SP = SP+-2;
    // Unit (Yielding)
    SP[2] = SP[3];
    SP[3] = SP[5];
    SP[5] = Ccolon;
    SP = SP+6;
    goto *(SP[-4]);

fun_not:
    SP = SP+-3;
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Call (Normal)
    SP[-1] = ((Pointer)(SP[5]))[3];
    SP[-2] = ((Pointer)(SP[5]))[2];
    SP[-3] = ((Pointer)(SP[5]))[1];
    SP[-4] = ((GrWord)(&&retlab8));
    SP[-5] = ((GrWord)(SP+3));
    SP = SP+-5;
    goto app2;
    retlab8:
    //  (inlined)
    // UpdateUnit
    SP[2] = ((Pointer)(SP[5]))[0] = SP[-1];
    // Case
    switch (SP[2]) {
        case CFalse:
            // Unit (Yielding)
            SP[5] = CTrue;
            SP = SP+6;
            goto *(SP[-2]);
            break;
        case CTrue:
            // Unit (Yielding)
            SP[5] = CFalse;
            SP = SP+6;
            goto *(SP[-2]);
            break;
    }

fun_ampamp:
    SP = SP+-3;
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Call (Normal)
    SP[-1] = ((Pointer)(SP[5]))[3];
    SP[-2] = ((Pointer)(SP[5]))[2];
    SP[-3] = ((Pointer)(SP[5]))[1];
    SP[-4] = ((GrWord)(&&retlab9));
    SP[-5] = ((GrWord)(SP+3));
    SP = SP+-5;
    goto app2;
    retlab9:
    //  (inlined)
    // UpdateUnit
    SP[2] = ((Pointer)(SP[5]))[0] = SP[-1];
    // Case
    switch (SP[2]) {
        case CFalse:
            // Unit (Yielding)
            SP[6] = CFalse;
            SP = SP+7;
            goto *(SP[-3]);
            break;
        case CTrue:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[6]))[3];
            SP[-2] = ((Pointer)(SP[6]))[2];
            SP[-3] = ((Pointer)(SP[6]))[1];
            SP[-4] = ((GrWord)(&&retlab10));
            SP[-5] = ((GrWord)(SP+3));
            SP = SP+-5;
            goto app2;
            retlab10:
            //  (inlined)
            // Update
            ((Pointer)(SP[6]))[0] = SP[-1];
            // Unit (Yielding)
            SP[6] = SP[-1];
            SP = SP+7;
            goto *(SP[-3]);
            break;
    }

fun_slasheq:
    SP = SP+-4;
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[6]))[0]) {
        case HOLE:
            // Unit (Internal)
            break;
        case Ffun__res_0_11_0__43_0tilde25:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[6]))[2];
            SP[-2] = ((Pointer)(SP[6]))[1];
            SP[-3] = ((GrWord)(&&retlab11));
            SP[-4] = ((GrWord)(SP+4));
            SP = SP+-4;
            goto fun__res_0_11_0__43_0tilde25;
            retlab11:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            SP[3] = ((Pointer)(SP[6]))[1] = SP[-2];
            ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case Ffun__res_0_9_0__59_0tilde40:
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[6]))[1];
            SP[-2] = ((GrWord)(&&retlab12));
            SP[-3] = ((GrWord)(SP+4));
            SP = SP+-3;
            goto fun__res_0_9_0__59_0tilde40;
            retlab12:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            SP[3] = ((Pointer)(SP[6]))[1] = SP[-2];
            ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case CDictminusEq:
            // Fetch
            //  (inlined)
            // Fetch
            SP[2] = ((Pointer)(SP[6]))[2];
            // Unit (Internal)
            SP[3] = ((Pointer)(SP[6]))[1];
            break;
    }
    // Fetch
    //  (inlined)
    // Unit (Yielding)
    SP[6] = P2_fun_10_0_46_0;
    SP[4] = SP[5];
    SP[5] = ((Pointer)(SP[3]))[1];
    SP = SP+7;
    goto *(SP[-3]);

fun_eqeq:
    SP = SP+-4;
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[6]))[0]) {
        case HOLE:
            // Unit (Internal)
            break;
        case Ffun_1_390_2_0:
            // Call (Normal)
            SP[-1] = ((GrWord)(&&retlab13));
            SP[-2] = ((GrWord)(SP+4));
            SP = SP+-2;
            goto fun_1_390_2_0;
            retlab13:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            ((Pointer)(SP[6]))[1] = SP[-2];
            SP[3] = ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case Ffun__res_0_11_0__43_0tilde25:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[6]))[2];
            SP[-2] = ((Pointer)(SP[6]))[1];
            SP[-3] = ((GrWord)(&&retlab14));
            SP[-4] = ((GrWord)(SP+4));
            SP = SP+-4;
            goto fun__res_0_11_0__43_0tilde25;
            retlab14:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            ((Pointer)(SP[6]))[1] = SP[-2];
            SP[3] = ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case Ffun__res_0_9_0__59_0tilde40:
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[6]))[1];
            SP[-2] = ((GrWord)(&&retlab15));
            SP[-3] = ((GrWord)(SP+4));
            SP = SP+-3;
            goto fun__res_0_9_0__59_0tilde40;
            retlab15:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            ((Pointer)(SP[6]))[1] = SP[-2];
            SP[3] = ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case Ffun_0_9_0:
            // Call (Normal)
            SP[-1] = ((GrWord)(&&retlab16));
            SP[-2] = ((GrWord)(SP+4));
            SP = SP+-2;
            goto fun_0_9_0;
            retlab16:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            ((Pointer)(SP[6]))[0] = SP[-1];
            ((Pointer)(SP[6]))[1] = SP[-2];
            SP[3] = ((Pointer)(SP[6]))[2] = SP[-3];
            break;
        case CDictminusEq:
            // Fetch
            SP[2] = ((Pointer)(SP[6]))[1];
            // Fetch
            //  (inlined)
            // Unit (Internal)
            SP[3] = ((Pointer)(SP[6]))[2];
            break;
    }
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[3]))[0]) {
        case P2_fun_primEqInt:
            // Unit (Yielding)
            SP[6] = P2_fun_primEqInt;
            SP = SP+7;
            goto *(SP[-2]);
            break;
        case P2_fun_10_0_85_0:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Unit (Yielding)
            SP[6] = P2_fun_10_0_85_0;
            auxVar = SP[3];
            SP[4] = ((Pointer)(SP[3]))[2];
            SP[3] = SP[5];
            SP[5] = ((Pointer)auxVar)[1];
            SP = SP+7;
            goto *(SP[-4]);
            break;
    }

fun_10_0_46_0:
    
    SP = SP+-4;
    // Store
    SP[3] = heapalloc(3);
    ((Pointer)(SP[3]))[0] = Ffun_eqeq;
    ((Pointer)(SP[3]))[1] = SP[6];
    // Store
    SP[2] = heapalloc(5);
    ((Pointer)(SP[2]))[0] = Aapp2;
    ((Pointer)(SP[2]))[1] = SP[3];
    ((Pointer)(SP[2]))[2] = SP[7];
    ((Pointer)(SP[2]))[3] = SP[8];
    // Call (Tail) 
    SP[8] = SP[2];
    SP[7] = SP[5];
    SP[6] = SP[4];
    SP = SP+6;
    goto fun_not;

fun_0_6_0:
    SP = SP+-5;
    // Store
    SP[3] = heapalloc(3);
    ((Pointer)(SP[3]))[0] = P2_fun_10_0_54_0;
    ((Pointer)(SP[3]))[1] = SP[7];
    // Store
    SP[2] = heapalloc(3);
    ((Pointer)(SP[2]))[0] = P2_fun_10_0_46_0;
    ((Pointer)(SP[2]))[1] = SP[7];
    // Store
    SP[4] = heapalloc(4);
    ((Pointer)(SP[4]))[0] = CDictminusEq;
    ((Pointer)(SP[4]))[1] = SP[2];
    ((Pointer)(SP[4]))[2] = SP[3];
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Unit (Yielding)
    SP[7] = CDictminusEq;
    auxVar = SP[4];
    SP[5] = ((Pointer)(SP[4]))[2];
    SP[4] = SP[6];
    SP[6] = ((Pointer)auxVar)[1];
    SP = SP+8;
    goto *(SP[-4]);

fun_1_390_2_0:
    
    SP = SP+-2;
    // Call (Tail) 
    SP[1] = SP[2];
    SP[2] = SP[3];
    SP[3] = global_0_9_0;
    SP = SP+1;
    goto fun_0_11_0;

fun_10_0_70:
    
    SP = SP+-2;
    // Store
    //  (inlined)
    // Call (Tail) 
    SP[1] = SP[3];
    SP[3] = global_subbus;
    SP[0] = SP[2];
    SP[2] = (( 100 )<<1)|1 ;
    SP = SP+0;
    goto fun_colon;

fun_10_0_68:
    
    SP = SP+-2;
    // Store
    //  (inlined)
    // Call (Tail) 
    SP[1] = SP[3];
    SP[3] = global_subbus;
    SP[0] = SP[2];
    SP[2] = (( 100 )<<1)|1 ;
    SP = SP+0;
    goto fun_colon;

fun_main:
    
    SP = SP+-2;
    // Call (Normal)
    SP[-1] = global_1_390_2_0;
    SP[-2] = ((GrWord)(&&retlab21));
    SP[-3] = ((GrWord)(SP+2));
    SP = SP+-3;
    goto fun_eqeq;
    retlab21:
    //  (inlined)
    //  (inlined)
    //  (inlined)
    // Case
    switch (SP[-1]) {
        case P2_fun_primEqInt:
            // Call (Tail) 
            SP[1] = SP[3];
            SP[3] = global_10_0_70;
            SP[0] = SP[2];
            SP[2] = global_10_0_68;
            SP = SP+0;
            goto fun_primEqInt;
            break;
        case P2_fun_10_0_85_0:
            // Call (Tail) 
            SP[-1] = SP[3];
            SP[3] = global_10_0_70;
            SP[0] = SP[-2];
            SP[-2] = SP[2];
            SP[2] = global_10_0_68;
            SP[1] = SP[-3];
            SP = SP+-2;
            goto fun_10_0_85_0;
            break;
    }

fun_10_0_85_0:
    SP = SP+-15;
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[19]))[0]) {
        case Ffun_10_0_68:
            // Call (Normal)
            SP[-1] = ((GrWord)(&&retlab24));
            SP[-2] = ((GrWord)(SP+15));
            SP = SP+-2;
            goto fun_10_0_68;
            retlab24:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // UpdateUnit
            SP[10] = ((Pointer)(SP[19]))[0] = SP[-1];
            SP[9] = ((Pointer)(SP[19]))[1] = SP[-2];
            SP[8] = ((Pointer)(SP[19]))[2] = SP[-3];
            break;
        case Ccolon:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Unit (Internal)
            SP[10] = Ccolon;
            SP[9] = ((Pointer)(SP[19]))[1];
            SP[8] = ((Pointer)(SP[19]))[2];
            break;
        case Csubbus:
            // Unit (Internal)
            SP[10] = Csubbus;
            break;
    }
    // Case
    switch (SP[10]) {
        case Ccolon:
            // Fetch
            //  (inlined)
            // Case
            switch (((Pointer)(SP[20]))[0]) {
                case Ffun_10_0_70:
                    // Call (Normal)
                    SP[-1] = ((GrWord)(&&retlab25));
                    SP[-2] = ((GrWord)(SP+15));
                    SP = SP+-2;
                    goto fun_10_0_70;
                    retlab25:
                    //  (inlined)
                    //  (inlined)
                    //  (inlined)
                    // UpdateUnit
                    SP[7] = ((Pointer)(SP[20]))[0] = SP[-1];
                    SP[6] = ((Pointer)(SP[20]))[1] = SP[-2];
                    SP[5] = ((Pointer)(SP[20]))[2] = SP[-3];
                    break;
                case Ccolon:
                    // Fetch
                    //  (inlined)
                    // Fetch
                    //  (inlined)
                    // Unit (Internal)
                    SP[7] = Ccolon;
                    SP[6] = ((Pointer)(SP[20]))[1];
                    SP[5] = ((Pointer)(SP[20]))[2];
                    break;
                case Csubbus:
                    // Unit (Internal)
                    SP[7] = Csubbus;
                    break;
            }
            // Case
            switch (SP[7]) {
                case Ccolon:
                    // Store
                    SP[14] = heapalloc(3);
                    ((Pointer)(SP[14]))[0] = Ffun_eqeq;
                    ((Pointer)(SP[14]))[1] = SP[18];
                    // Store
                    SP[13] = heapalloc(5);
                    ((Pointer)(SP[13]))[0] = Aapp2;
                    ((Pointer)(SP[13]))[1] = SP[14];
                    ((Pointer)(SP[13]))[2] = SP[8];
                    ((Pointer)(SP[13]))[3] = SP[5];
                    // Store
                    SP[12] = heapalloc(3);
                    ((Pointer)(SP[12]))[0] = Ffun_eqeq;
                    ((Pointer)(SP[12]))[1] = SP[17];
                    // Store
                    SP[11] = heapalloc(5);
                    ((Pointer)(SP[11]))[0] = Aapp2;
                    ((Pointer)(SP[11]))[1] = SP[12];
                    ((Pointer)(SP[11]))[2] = SP[9];
                    ((Pointer)(SP[11]))[3] = SP[6];
                    // Call (Tail) 
                    SP[20] = SP[13];
                    SP[19] = SP[11];
                    SP[18] = SP[16];
                    SP[17] = SP[15];
                    SP = SP+17;
                    goto fun_ampamp;
                    break;
                case Csubbus:
                    // Unit (Yielding)
                    SP[20] = CFalse;
                    SP = SP+21;
                    goto *(SP[-5]);
                    break;
            }
            break;
        case Csubbus:
            // Fetch
            //  (inlined)
            // Case
            switch (((Pointer)(SP[20]))[0]) {
                case Ffun_10_0_70:
                    // Call (Normal)
                    SP[-1] = ((GrWord)(&&retlab27));
                    SP[-2] = ((GrWord)(SP+15));
                    SP = SP+-2;
                    goto fun_10_0_70;
                    retlab27:
                    //  (inlined)
                    //  (inlined)
                    //  (inlined)
                    // UpdateUnit
                    SP[4] = ((Pointer)(SP[20]))[0] = SP[-1];
                    ((Pointer)(SP[20]))[1] = SP[-2];
                    ((Pointer)(SP[20]))[2] = SP[-3];
                    break;
                case Ccolon:
                    // Fetch
                    SP[2] = ((Pointer)(SP[20]))[1];
                    // Fetch
                    SP[3] = ((Pointer)(SP[20]))[2];
                    // Unit (Internal)
                    SP[4] = Ccolon;
                    break;
                case Csubbus:
                    // Unit (Internal)
                    SP[4] = Csubbus;
                    break;
            }
            // Case
            switch (SP[4]) {
                case Ccolon:
                    // Unit (Yielding)
                    SP[20] = CFalse;
                    SP = SP+21;
                    goto *(SP[-5]);
                    break;
                case Csubbus:
                    // Unit (Yielding)
                    SP[20] = CTrue;
                    SP = SP+21;
                    goto *(SP[-5]);
                    break;
            }
            break;
    }

fun__res_0_11_0__43_0tilde25:
    SP = SP+-5;
    // Call (Normal)
    SP[-1] = SP[8];
    SP[-2] = ((GrWord)(&&retlab28));
    SP[-3] = ((GrWord)(SP+5));
    SP = SP+-3;
    goto fun_0_6_0;
    retlab28:
    //  (inlined)
    //  (inlined)
    // Store
    SP[3] = heapalloc(4);
    ((Pointer)(SP[3]))[0] = CDictminusEq;
    ((Pointer)(SP[3]))[1] = SP[-2];
    ((Pointer)(SP[3]))[2] = SP[-3];
    // Fetch
    //  (inlined)
    // Store
    SP[2] = heapalloc(4);
    ((Pointer)(SP[2]))[0] = P2_fun_10_0_85_0;
    ((Pointer)(SP[2]))[1] = SP[7];
    ((Pointer)(SP[2]))[2] = SP[8];
    // Store
    SP[4] = heapalloc(4);
    ((Pointer)(SP[4]))[0] = CDictminusEq;
    ((Pointer)(SP[4]))[1] = ((Pointer)(SP[3]))[1];
    ((Pointer)(SP[4]))[2] = SP[2];
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Unit (Yielding)
    SP[8] = CDictminusEq;
    SP[7] = ((Pointer)(SP[4]))[1];
    SP[5] = SP[6];
    SP[6] = ((Pointer)(SP[4]))[2];
    SP = SP+9;
    goto *(SP[-4]);

fun_0_11_0:
    SP = SP+-4;
    // Store
    SP[3] = heapalloc(3);
    ((Pointer)(SP[3]))[0] = HOLE;
    // Store
    SP[2] = heapalloc(4);
    ((Pointer)(SP[2]))[0] = Ffun__res_0_11_0__43_0tilde25;
    ((Pointer)(SP[2]))[1] = SP[6];
    ((Pointer)(SP[2]))[2] = SP[3];
    // FetchUpdate
    ((Pointer)(SP[3]))[0] = ((Pointer)(SP[2]))[0];
    ((Pointer)(SP[3]))[1] = ((Pointer)(SP[2]))[1];
    ((Pointer)(SP[3]))[2] = ((Pointer)(SP[2]))[2];
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[3]))[0]) {
        case HOLE:
            // Unit (Yielding)
            SP[6] = HOLE;
            SP = SP+7;
            goto *(SP[-2]);
            break;
        case Ffun__res_0_11_0__43_0tilde25:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[3]))[2];
            SP[-2] = ((Pointer)(SP[3]))[1];
            SP[-3] = ((GrWord)(&&retlab29));
            SP[-4] = ((GrWord)(SP+4));
            SP = SP+-4;
            goto fun__res_0_11_0__43_0tilde25;
            retlab29:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // Update
            ((Pointer)(SP[3]))[0] = SP[-1];
            ((Pointer)(SP[3]))[1] = SP[-2];
            ((Pointer)(SP[3]))[2] = SP[-3];
            // Unit (Yielding)
            SP[6] = SP[-1];
            SP[3] = SP[5];
            SP[5] = SP[-2];
            SP[4] = SP[-3];
            SP = SP+7;
            goto *(SP[-4]);
            break;
        case CDictminusEq:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Unit (Yielding)
            SP[6] = CDictminusEq;
            auxVar = SP[3];
            SP[4] = ((Pointer)(SP[3]))[2];
            SP[3] = SP[5];
            SP[5] = ((Pointer)auxVar)[1];
            SP = SP+7;
            goto *(SP[-4]);
            break;
    }

fun__res_0_9_0__59_0tilde40:
    SP = SP+-5;
    // Call (Normal)
    SP[-1] = SP[7];
    SP[-2] = ((GrWord)(&&retlab30));
    SP[-3] = ((GrWord)(SP+5));
    SP = SP+-3;
    goto fun_0_6_0;
    retlab30:
    //  (inlined)
    //  (inlined)
    // Store
    SP[3] = heapalloc(4);
    ((Pointer)(SP[3]))[0] = CDictminusEq;
    ((Pointer)(SP[3]))[1] = SP[-2];
    ((Pointer)(SP[3]))[2] = SP[-3];
    // Fetch
    //  (inlined)
    // Store
    SP[2] = heapalloc(2);
    ((Pointer)(SP[2]))[0] = P2_fun_primEqInt;
    // Store
    SP[4] = heapalloc(4);
    ((Pointer)(SP[4]))[0] = CDictminusEq;
    ((Pointer)(SP[4]))[1] = ((Pointer)(SP[3]))[1];
    ((Pointer)(SP[4]))[2] = SP[2];
    // Fetch
    //  (inlined)
    // Fetch
    //  (inlined)
    // Unit (Yielding)
    SP[7] = CDictminusEq;
    auxVar = SP[4];
    SP[5] = ((Pointer)(SP[4]))[2];
    SP[4] = SP[6];
    SP[6] = ((Pointer)auxVar)[1];
    SP = SP+8;
    goto *(SP[-4]);

fun_0_9_0:
    SP = SP+-4;
    // Store
    SP[3] = heapalloc(3);
    ((Pointer)(SP[3]))[0] = HOLE;
    // Store
    SP[2] = heapalloc(3);
    ((Pointer)(SP[2]))[0] = Ffun__res_0_9_0__59_0tilde40;
    ((Pointer)(SP[2]))[1] = SP[3];
    // FetchUpdate
    ((Pointer)(SP[3]))[0] = ((Pointer)(SP[2]))[0];
    ((Pointer)(SP[3]))[1] = ((Pointer)(SP[2]))[1];
    ((Pointer)(SP[3]))[2] = ((Pointer)(SP[2]))[2];
    // Fetch
    //  (inlined)
    // Case
    switch (((Pointer)(SP[3]))[0]) {
        case HOLE:
            // Unit (Yielding)
            SP[4] = SP[5];
            SP[5] = HOLE;
            SP = SP+6;
            goto *(SP[-2]);
            break;
        case Ffun__res_0_9_0__59_0tilde40:
            // Fetch
            //  (inlined)
            // Call (Normal)
            SP[-1] = ((Pointer)(SP[3]))[1];
            SP[-2] = ((GrWord)(&&retlab31));
            SP[-3] = ((GrWord)(SP+4));
            SP = SP+-3;
            goto fun__res_0_9_0__59_0tilde40;
            retlab31:
            //  (inlined)
            //  (inlined)
            //  (inlined)
            // Update
            ((Pointer)(SP[3]))[0] = SP[-1];
            ((Pointer)(SP[3]))[1] = SP[-2];
            ((Pointer)(SP[3]))[2] = SP[-3];
            // Unit (Yielding)
            SP[2] = SP[5];
            SP[5] = SP[-1];
            SP[4] = SP[-2];
            SP[3] = SP[-3];
            SP = SP+6;
            goto *(SP[-4]);
            break;
        case CDictminusEq:
            // Fetch
            //  (inlined)
            // Fetch
            //  (inlined)
            // Unit (Yielding)
            SP[2] = SP[5];
            SP[5] = CDictminusEq;
            SP[4] = ((Pointer)(SP[3]))[1];
            SP = SP+6;
            goto *(SP[-4]);
            break;
    }

}
