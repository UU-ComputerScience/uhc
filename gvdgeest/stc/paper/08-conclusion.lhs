\section{Conclusion}
\label{conclusion}
We have shown a systematic approach to embed languages in Haskell.
All the systematic translations use the basic framework of modeling postfix function application.
Although we have not: it is also possible to embed languages based on LR(0) and LR(1) grammars using the same framework. 
A nice feature of the embeddings is that type checking is used to guarantee that the embedding is syntactically correct. 
The downside is that syntactic errors are given as type-errors: a solution would be to script specialized type error messages for an embedding~\cite{Helscripting}.
Another maybe surprising result is that the industrial strength compiler GHC~\cite{Ghc} has serious performance problems with compiling quotations which consists of more then 20 terminals. 
