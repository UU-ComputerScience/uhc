{- --------------------------------------------------------------------------
// Macros to help make various instances for tuples.
// A kind of poor mans deriving.
//
// --------------------------------------------------------------------------
-}

#define	COMMA	,

{- --------------------------------------------------------------------------
// 2 tuple, 1 unary op
// --------------------------------------------------------------------------
-}
#define TUPLE2_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
instance (clscon a, clscon b) => clscon (a,b) where \
  { op1 op1preArg (a,b) op1postArg \
      = pre \
            op1subop a \
        sep op1subop b \
        post \
  }

{- --------------------------------------------------------------------------
// 3 tuple, 1 unary op
// --------------------------------------------------------------------------
-}
#define TUPLE3_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c) => clscon (a,b,c) where \
  { op1 op1preArg (a,b,c) op1postArg \
      = pre \
            op1subop a \
        sep op1subop b \
        sep op1subop c \
        post \
  }

{- --------------------------------------------------------------------------
// 4 tuple, 1 unary op
// --------------------------------------------------------------------------
-}
#define TUPLE4_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c, clscon d) => clscon (a,b,c,d) where \
  { op1 op1preArg (a,b,c,d) op1postArg \
      = pre \
            op1subop a \
        sep op1subop b \
        sep op1subop c \
        sep op1subop d \
        post \
  }

{- --------------------------------------------------------------------------
// 5 tuple, 1 unary op
// --------------------------------------------------------------------------
-}
#define TUPLE5_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c, clscon d, clscon e) => clscon (a,b,c,d,e) where \
  { op1 op1preArg (a,b,c,d,e) op1postArg \
      = pre \
            op1subop a \
        sep op1subop b \
        sep op1subop c \
        sep op1subop d \
        sep op1subop e \
        post \
  }

{- --------------------------------------------------------------------------
// Set of 2..n tuple, 1 unary op
// --------------------------------------------------------------------------
-}
#define TUPLE_UNOP1_INSTANCES(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
{ TUPLE2_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) ;\
  TUPLE3_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) ;\
  TUPLE4_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) ;\
  TUPLE5_UNOP1_INSTANCE(clscon,op1,op1preArg,op1postArg,op1subop,pre,sep,post) \
}

{- --------------------------------------------------------------------------
// 2 tuple, 1 binary op
// --------------------------------------------------------------------------
-}
#define TUPLE2_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) \
instance (clscon a, clscon b) => clscon (a,b) where \
  { (a1,b1) op1 (a2,b2) \
      = pre \
            a1 op1subop a2 \
        sep b1 op1subop b2 \
        post \
  }

{- --------------------------------------------------------------------------
// 3 tuple, 1 binary op
// --------------------------------------------------------------------------
-}
#define TUPLE3_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c) => clscon (a,b,c) where \
  { (a1,b1,c1) op1 (a2,b2,c2) \
      = pre \
            a1 op1subop a2 \
        sep b1 op1subop b2 \
        sep c1 op1subop c2 \
        post \
  }

{- --------------------------------------------------------------------------
// 4 tuple, 1 binary op
// --------------------------------------------------------------------------
-}
#define TUPLE4_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c, clscon d) => clscon (a,b,c,d) where \
  { (a1,b1,c1,d1) op1 (a2,b2,c2,d2) \
      = pre \
            a1 op1subop a2 \
        sep b1 op1subop b2 \
        sep c1 op1subop c2 \
        sep d1 op1subop d2 \
        post \
  }

{- --------------------------------------------------------------------------
// 5 tuple, 1 binary op
// --------------------------------------------------------------------------
-}
#define TUPLE5_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) \
instance (clscon a, clscon b, clscon c, clscon d, clscon e) => clscon (a,b,c,d,e) where \
  { (a1,b1,c1,d1,e1) op1 (a2,b2,c2,d2,e2) \
      = pre \
            a1 op1subop a2 \
        sep b1 op1subop b2 \
        sep c1 op1subop c2 \
        sep d1 op1subop d2 \
        sep e1 op1subop e2 \
        post \
  }

{- --------------------------------------------------------------------------
// Set of 2..n tuple, 1 binary op
// --------------------------------------------------------------------------
-}
#define TUPLE_BINOP1_INSTANCES(clscon,op1,op1subop,pre,sep,post) \
{ TUPLE2_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) ;\
  TUPLE3_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) ;\
  TUPLE4_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) ;\
  TUPLE5_BINOP1_INSTANCE(clscon,op1,op1subop,pre,sep,post) \
}

