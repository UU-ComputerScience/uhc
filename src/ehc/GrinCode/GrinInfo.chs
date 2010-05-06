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


%%[(8 codegen grin) hs export(GrinInfo, emptyGrinInfo)

data GrinInfo = GrinInfo
  { crap :: ()
%%[[9
  , grMbMergeInstance :: Maybe InfoMergeInstance
%%]]
  }

emptyGrinInfo :: GrinInfo
emptyGrinInfo = GrinInfo
  { crap = ()
%%[[9
  , grMbMergeInstance = Nothing
%%]]
  }

%%]


%%[(8 codegen grin) hs export(GrinInfoPart(..),grinInfoMergeInstance)

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

%%]
