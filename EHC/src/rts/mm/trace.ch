%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Trace traces objects, knowing the particulars of object layout, Space use, etc.
It requires a MM_TraceSupply for possibly queueing new work, and an allocator to possibly allocate for a copy.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef QuartWord 				MM_Trace_Flg ;

#define	MM_Trace_Flg_Copy		(1<<0)			// copy inspected object
#define	MM_Trace_Flg_Trace		(1<<1)			// trace content of object 1 level

#define	MM_Trace_Flg_All		(MM_Trace_Flg_Copy | MM_Trace_Flg_Trace)

%%]

%%[8
typedef Ptr  MM_Trace_Data_Priv ;

typedef struct MM_Trace {
	// private data of Trace
  	MM_Trace_Data_Priv 			data ;
  	
	// private data, but included here for fast access, or always required to be present
  	MM_Allocator*	 			allocator ;			// for copying
  	MM_Collector*	 			collector ;
  	Word						objectHeaderNrWords ;

  	// setup with
  	// - supply to put new traceable objects init
  	// - allocator to make copies
  	// - collector of which this trace is part
  	// note: correct type for traceSupply cannot be given because of mutual recursiveness between MM_Trace and MM_TraceSupply
  	void			 			(*init)( struct MM_Trace*, /* struct MM_TraceSupply* */ Ptr traceSupply, MM_Allocator* allocator, MM_Collector* collector ) ;
  	
  	// is obj traceable?
  	Bool			 			(*canTraceObject)( struct MM_Trace*, Word obj ) ;
  	
  	// trace a single object, return new object
  	// assumption: canTraceObject( , obj ) == True
  	Word			 			(*traceObject)( struct MM_Trace*, Word obj, MM_Trace_Flg flg ) ;
  	
  	// trace multiple objects, replace by new objects
  	// check on traceability is done by function
  	void			 			(*traceObjects)( struct MM_Trace*, Word* objs, Word nrObjs, MM_Trace_Flg flg ) ;
  	
  	// size of an object in words
  	Word			 			(*objectNrWords)( struct MM_Trace*, Word obj ) ;
  	
} MM_Trace ;
%%]

%%[8
static inline Word mm_Trace_TraceObject( MM_Trace* trace, Word obj, MM_Trace_Flg flg ) {
	// printf("mm_Trace_TraceObject obj=%x space(obj)=%x space=%x\n",obj,mm_Spaces_GetSpaceForAddress(obj),trace->collector->collectedSpace);
	if ( trace->canTraceObject( trace, obj ) ) {
		return trace->traceObject( trace, obj, flg ) ;
	} else {
		return obj ;
	}
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Trace mm_trace ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_trace() ;
%%]
