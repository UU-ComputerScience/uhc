%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Trace traces objects, knowing the particulars of object layout, Space use, etc.
It requires a MM_TraceSupply for possibly queueing new work, and an allocator to possibly allocate for a copy.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Trace_Data_Priv ;

typedef struct MM_Trace {
	// private data of Trace
  	MM_Trace_Data_Priv 			data ;
  	
	// private data, but included here for fast access
  	MM_Collector*	 			collector ;

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
  	Word			 			(*traceObject)( struct MM_Trace*, Word obj ) ;
  	
} MM_Trace ;
%%]

%%[8
static inline Word mm_Trace_TraceObject( MM_Trace* trace, Word obj ) {
	if ( trace->canTraceObject( trace, obj ) ) {
		return trace->traceObject( trace, obj ) ;
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
