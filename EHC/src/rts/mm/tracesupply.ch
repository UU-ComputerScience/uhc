%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A TraceSupply knows what to trace (by means of a Trace)
To a TraceSupply work can be added (for later scanning), or retrieved (for immediate scanning).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TraceSupply interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_TraceSupply_Data_Priv ;

typedef struct MM_TraceSupply {
	// private data of TraceSupply
  	MM_TraceSupply_Data_Priv 	data ;
  	
  	// setup, alternatively with sub traceSupplies depending on traceSupply
  	void			 			(*init)( struct MM_TraceSupply*, MM_Malloc* memmgt, struct MM_Mutator* ) ;
  	void			 			(*initWithSub)( struct MM_TraceSupply*, MM_Malloc* memmgt, struct MM_Mutator*, MM_FlexArray* subTraceSupplies ) ;
  	
  	// reset to initial state for tracing another time
  	void			 			(*reset)( struct MM_TraceSupply*, Word gcInfo ) ;
  	
  	// run trace to completion
  	void			 			(*run)( struct MM_TraceSupply* ) ;
  	
  	// adding work
  	// the use&meaning of 'extra' is determined by a particular combination of trace pushing work and its tracesupply
  	void						(*pushWork)( struct MM_TraceSupply*, Word* work, Word nrWorkWords, Word extra ) ;
  	
  	// tracing live pointers
  	
  	// collection
} MM_TraceSupply ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default TraceSupply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_TraceSupply mm_traceSupply ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_traceSupply() ;
%%]
