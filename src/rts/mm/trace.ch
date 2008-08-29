%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Trace traces objects for a particular Plan.
To a Trace work can be added (for later scanning), or retrieved (for immediate scanning).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Trace_Data_Priv ;

typedef struct MM_Trace {
	// private data of Trace
  	MM_Trace_Data_Priv 		data ;
  	
  	// setup, alternatively with sub traces depending on trace
  	void			 			(*init)( struct MM_Trace* ) ;
  	void			 			(*initWithSub)( struct MM_Trace*, MM_FlexArray* subTraces ) ;
  	
  	// iterator like interface for getting/adding work
  	Word						(*popWork)( struct MM_Trace*, Word* work, Word nrWorkWords ) ;
  	void						(*pushWork)( struct MM_Trace*, Word* work, Word nrWorkWords ) ;
  	
  	// tracing live pointers
  	
  	// collection
} MM_Trace ;
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
