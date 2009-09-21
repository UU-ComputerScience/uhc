%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: range map
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Range Map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_RangeMap* mm_rangeMap_New( MM_Malloc* memmgt, MM_RangeMap* a ) {
	if ( a == NULL ) {
		a = (MM_RangeMap*) memmgt->malloc( sizeof( MM_RangeMap ) ) ;
		a->didAlcDescr = True ;
	} else {
		a->didAlcDescr = False ;
	}
	a->ptr = NULL ;
	a->didAlcPtr = False ;
	a->firstInx = 0 ;
	a->size = 0 ;
	a->memMgt = memmgt ;
	// mm_rangeMap_Realloc( a, NULL, firstInx, aftLastInx ) ;
}

void mm_rangeMap_Free( MM_RangeMap* a ) {
	if ( a->didAlcPtr ) {
		a->memMgt->free( a->ptr ) ;
	}
	if ( a->didAlcDescr ) {
		a->memMgt->free( a ) ;
	}
}

// assume this does not happen too often, so allocate exactly what is asked for
// assume it only grows
void mm_rangeMap_Realloc( MM_RangeMap* a, WPtr ptr, MM_RangeMap_Inx firstInx, MM_RangeMap_Inx aftLastInx ) {
	// IF_GB_TR_ON(3,{printf("mm_rangeMap_Realloc a=%p p=%p firstInx=%x aftLastInx=%x a->firstInx=%x a->size=%x\n", a, a->ptr, firstInx, aftLastInx, a->firstInx, a->size);}) ;
	if ( a->firstInx == 0 ) {
		a->firstInx = firstInx ;
	} else {
		firstInx = minWord( a->firstInx, firstInx ) ;
	}
	aftLastInx = maxWord( aftLastInx, a->firstInx + a->size ) ;
	MM_RangeMap_Inx szNew = aftLastInx - firstInx ;
	
	if ( szNew > a->size || ptr != NULL ) {
		WPtr ptrNew ;
		Word szNewBytes = szNew << Word_SizeInBytes_Log ;
		Bool didAlcPtrNew ;
		
		// if given a ptr, use that one
		if ( ptr == NULL ) {
			ptrNew = a->memMgt->malloc( szNewBytes ) ;
			didAlcPtrNew = True ;
		} else {
			ptrNew = ptr ;
			didAlcPtrNew = False ;
		}
		
		// init & copy old values
		int i ;
		for ( i = firstInx ; i < a->firstInx ; i++ ) {
			ptrNew[ i - firstInx ] = 0 ;
		}
		for ( i = a->firstInx ; i < (a->size + a->firstInx) ; i++ ) {
			ptrNew[ i - firstInx ] = a->ptr[ i - a->firstInx ] ;
		}
		for ( i = a->size + a->firstInx ; i < aftLastInx ; i++ ) {
			ptrNew[ i - firstInx ] = 0 ;
		}

		// deallocate old, assign new
		if ( a->didAlcPtr ) {
			a->memMgt->free( a->ptr ) ;
		}
		a->ptr = ptrNew ;
		a->didAlcPtr = didAlcPtrNew ;
		
		// set new size & first inx
		a->firstInx = firstInx ;
		a->size = szNew ;
		// IF_GB_TR_ON(3,{printf("mm_rangeMap_Realloc a=%p p=%p firstInx=%x aftLastInx=%x a->firstInx=%x a->size=%x\n", a, a->ptr, firstInx, aftLastInx, a->firstInx, a->size);}) ;
		// IF_GB_TR_ON(3,{printf("mm_rangeMap_Realloc szNew=%x szNewBytes=%x\n", szNew, szNewBytes);}) ;
	}
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_rangeMap_Dump( MM_RangeMap* a ) {
	printf( ">------------------------> RangeMap\n" ) ;

	int i ;
	for ( i = mm_rangeMap_FirstInx(a) ; i < mm_rangeMap_AfterLastInx(a) ; i++ ) {
		printf( "  Rng %x: val=%x\n", i, *mm_rangeMap_At(a,i) ) ;
	}

	printf( "<------------------------< RangeMap\n" ) ;
}
#endif
%%]

