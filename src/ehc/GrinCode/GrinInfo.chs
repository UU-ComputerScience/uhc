%%[(8 codegen grin) hs module {%{EH}GrinCode.GrinInfo}
%%]

This module defines GrinInfo, a data structure that encapsulates all pieces of
information incremental Grin transformations need.

Concretely, a value of type GrinInfo contains information collected by different
transformations from a certain Grin module A. Transformations on a module that
imports module A can use this information instead of collecting it from the
whole program.

This Haskell module is oblivious to what 'information' actually means; this is
up to the transformations themselves.

-- Import incremental transformations.
%%[(9 codegen grin) hs import({%{EH}GrinCode.Trf.MergeInstance})
%%]
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.MemberSelect}, {%{EH}GrinCode.Trf.SimpleNullary}, {%{EH}GrinCode.Trf.EvalStored}, {%{EH}GrinCode.Trf.CleanupPass})
%%]


%%[(8 codegen grin) hs export(GrinInfo, emptyGrinInfo)

data GrinInfo = GrinInfo
  { grMbMemberSelect      :: Maybe InfoMemberSelect
  , grMbMemberSelectSpec  :: [Maybe InfoMemberSelect]
  , grMbSimpleNullary     :: Maybe InfoSimpleNullary
  , grMbSimpleNullarySpec :: [Maybe InfoSimpleNullary]
  , grMbEvalStoredSpec    :: [Maybe InfoEvalStored]
  , grMbCleanupPass       :: Maybe InfoCleanupPass
%%[[9
  , grMbMergeInstance     :: Maybe InfoMergeInstance
%%]]
  } deriving Show

emptyGrinInfo :: GrinInfo
emptyGrinInfo = GrinInfo
  { grMbMemberSelect      = Nothing
  , grMbMemberSelectSpec  = []
  , grMbSimpleNullary     = Nothing
  , grMbSimpleNullarySpec = []
  , grMbEvalStoredSpec    = []
  , grMbCleanupPass       = Nothing
%%[[9
  , grMbMergeInstance     = Nothing
%%]]
  }

%%]


%%[(9 codegen grin) hs export(grinInfoMergeInstance)
%%]

%%[(8 codegen grin) hs export(GrinInfoPart(..),grinInfoMemberSelect,grinInfoMemberSelectSpec,grinInfoSimpleNullary,grinInfoSimpleNullarySpec,grinInfoEvalStoredSpec,grinInfoCleanupPass)

type GrinInfoUpd i = i -> GrinInfo -> GrinInfo

-- GrinInfoPart encapsulates a way to retreive/update a part of the GrinInfo.
-- Typically, there should be a GrinInfoPart for each incremental
-- transformation.
data GrinInfoPart i = GrinInfoPart
  { grinInfoGet :: GrinInfo -> Maybe i
  , grinInfoUpd :: GrinInfoUpd i
  }

-- GrinInfoParts for each transformation.
%%[[9
grUpdMergeInstance :: GrinInfoUpd InfoMergeInstance
grUpdMergeInstance x sem = sem { grMbMergeInstance = Just x }

grinInfoMergeInstance = GrinInfoPart grMbMergeInstance grUpdMergeInstance
%%]]

grUpdMemberSelect :: GrinInfoUpd InfoMemberSelect
grUpdMemberSelect x sem = sem { grMbMemberSelect = Just x }

grinInfoMemberSelect = GrinInfoPart grMbMemberSelect grUpdMemberSelect
grinInfoMemberSelectSpec = grinInfoSpec grMbMemberSelectSpec (\x sem -> sem { grMbMemberSelectSpec = x })

grUpdSimpleNullary x sem = sem { grMbSimpleNullary = Just x }

grinInfoSimpleNullary = GrinInfoPart grMbSimpleNullary grUpdSimpleNullary
grinInfoSimpleNullarySpec = grinInfoSpec grMbSimpleNullarySpec (\x sem -> sem { grMbSimpleNullarySpec = x })

grinInfoEvalStoredSpec = grinInfoSpec grMbEvalStoredSpec (\x sem -> sem { grMbEvalStoredSpec = x })

grinInfoCleanupPass = GrinInfoPart grMbCleanupPass (\x sem -> sem { grMbCleanupPass = Just x })

%%]


%%[(8 codegen grin) hs
grinInfoSpec :: (GrinInfo -> [Maybe a]) -> ([Maybe a] -> GrinInfo -> GrinInfo) -> Int -> GrinInfoPart a
grinInfoSpec get upd i = GrinInfoPart
  { grinInfoGet = lget i . get
  , grinInfoUpd = \x inf -> (`upd` inf) . lput i x . get $ inf
  }

lget :: Int -> [Maybe a] -> Maybe a
lget i xs | i < length xs = xs !! i
          | otherwise     = Nothing

lput :: Int -> a -> [Maybe a] -> [Maybe a]
lput i x xs = case splitAt i xs' of
  (ll, [])     -> ll ++ [Just x]
  (ll, (_:lr)) -> ll ++ Just x : lr
  where xs' = xs ++ replicate (i - length xs) Nothing

%%]
