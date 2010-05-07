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
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.MemberSelect})
%%]


%%[(8 codegen grin) hs export(GrinInfo, emptyGrinInfo)

data GrinInfo = GrinInfo
  { grMbMemberSelect     :: Maybe InfoMemberSelect
  , grMbMemberSelectSpec :: [Maybe InfoMemberSelect]
%%[[9
  , grMbMergeInstance :: Maybe InfoMergeInstance
%%]]
  } deriving Show

emptyGrinInfo :: GrinInfo
emptyGrinInfo = GrinInfo
  { grMbMemberSelect  = Nothing
  , grMbMemberSelectSpec = []
%%[[9
  , grMbMergeInstance = Nothing
%%]]
  }

%%]


%%[(8 codegen grin) hs export(GrinInfoPart(..),grinInfoMergeInstance,grinInfoMemberSelect,grinInfoMemberSelectSpec)

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

grinInfoMemberSelectSpec :: Int -> GrinInfoPart InfoMemberSelect
grinInfoMemberSelectSpec i = GrinInfoPart
  { grinInfoGet = lget i . grMbMemberSelectSpec
  , grinInfoUpd = upd
  }
  where upd x sem = sem { grMbMemberSelectSpec = lput i x (grMbMemberSelectSpec sem) }


lget :: Int -> [Maybe a] -> Maybe a
lget i xs | i < length xs = xs !! i
          | otherwise     = Nothing

lput :: Int -> a -> [Maybe a] -> [Maybe a]
lput i x xs = case splitAt i xs' of
  (ll, [])     -> ll ++ [Just x]
  (ll, (_:lr)) -> ll ++ Just x : lr
  where xs' = xs ++ replicate (i - length xs) Nothing

%%]
