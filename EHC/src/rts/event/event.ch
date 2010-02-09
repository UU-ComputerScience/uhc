%%[8
#ifndef __EVENT_EVENT_H__
#define __EVENT_EVENT_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Events are the interface to anything which is supposed to generate some info for observing
the RTS:
\begin{itemize}
\item Tracing
\item Statistics
\item and later: any other messages (logging, ...)
\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Event interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  Event_Data_Priv ;

typedef struct Event {
	// private data of Event
  	Event_Data_Priv 		data ;
  	
  	// setup with a particular Event
  	void			 			(*init)( struct Event*, char* nameEvTy, Sys_Malloc* memmgt ) ;
  	
  	// event happening
  	void			 			(*ev1)( struct Event*, char* nameEv, Word x1 ) ;
  	void			 			(*ev2)( struct Event*, char* nameEv, Word x1, Word x2 ) ;
  	void			 			(*ev3)( struct Event*, char* nameEv, Word x1, Word x2, Word x3 ) ;
  	void			 			(*ev4)( struct Event*, char* nameEv, Word x1, Word x2, Word x3, Word x4 ) ;
  	
  	// sync with whatever needs to be synced (i.e. flush, gather, group, ...)
  	void			 			(*sync)( struct Event* ) ;
} Event ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Event
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
extern Event mm_xxx ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
extern void mm_init_xxx() ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __EVENT_EVENT_H__ */
%%]
