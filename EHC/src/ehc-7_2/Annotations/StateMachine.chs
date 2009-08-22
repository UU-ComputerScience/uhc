%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State Machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}Annotations.StateMachine}
%%]

%%[7_2 import(qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set))
%%]

%%[7_2 export(StateMachine, run, accept, empty, addTrans, addListTrans, addEndState)
%%]


Implementation of a non-deterministic state machine. We model a state machine
as a set of transitions and a set of start states. A transition a -x-> b, is
stored as a mapping from a to a mapping from x to b.

%%[7_2.data

newtype StateMachine s t = SM (Transitions s t, Set s) deriving Show
type Transitions s t = Map s (Map t (Set s))

%%]


The run function applies the state machine to a series of input tokens,
resulting in a set of possible end-states. If there is no parse, the set of
end states is empty. The accept function does the same and checks if at least one
of the resulting set of states is in a given set of end states.

%%[7_2.use

run :: (Ord s, Ord t) => StateMachine s t -> s -> [t] -> Set s
run (SM (trs, endSs)) initS
  = foldl (flip step) (Set.singleton initS)
  where step t = Set.unions . catMaybes . map (\s -> Map.lookup s trs >>= Map.lookup t) . Set.toList

accept :: (Ord s, Ord t) => StateMachine s t -> s -> [t] -> Bool
accept sm@(SM (_, endSs)) initS = not . Set.null . (`Set.intersection` endSs) . run sm initS

%%]


%%[7_2.build

empty :: StateMachine s t
empty = SM (Map.empty, Set.empty)

addTrans :: (Ord s, Ord t) => s -> t -> s -> StateMachine s t -> StateMachine s t
addTrans fromS tk toS (SM (trs, endSs))
  = SM (Map.insertWith update fromS single trs, endSs)
  where
    single = Map.singleton tk (Set.singleton toS)
    update = Map.unionWith Set.union

addListTrans :: (Ord s, Ord t) => s -> [t] -> s -> StateMachine s t -> StateMachine s t
addListTrans fromS tks toS sm = foldr (\tk -> addTrans fromS tk toS) sm tks

addEndState :: (Ord s, Ord t) => s -> StateMachine s t -> StateMachine s t
addEndState endS (SM (trs, endSs))
  = SM (trs, Set.insert endS endSs)

%%]
