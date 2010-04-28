# all variants
TEXT_PUB_VARIANTS			+= ruler-doc ehc-book ehc-doc
TEXT_DOC_VARIANTS			+= shuffle-doc text2text-doc \
								howtodoc-doc howtoexperiment-doc \
								ehc-technical-doc ehc-structure-doc ehc-user-doc \
								build-system-doc \
								getting-started-doc announce-doc release-history-doc \
								roadmap-doc
TEXT_PRIV_VARIANTS			+= flops06-ruler-paper flops06-ruler \
								popl07-explimpl \
								hw06-impred esop07-impred esop07-impred-tr \
								truu-explimpl truu-ruler \
								phd-paper phd-draft phd-tst phd \
								ehc-book-tst \
								scratch scratch2 \
								poster posterLDL posterTrOrPr poster-uhcarch \
								slides-ruler slides-ruler-long \
								slides-explimpl slides-explimpl-fpnl slides-overview slides-status \
								slides-ehcstruct slides-ehcstruct-ufmg slides-hs09-uhcarch \
								gbm \
								uniqueness slides-uniqueness \
								icfp07-chr-locinst icfp07-chr-locinst-blind cc08-chr-locinst \
								icfp07-ehcstruct icfp07-ehcstruct-blind ifl07-ehcstruct \
								icfp08-subst padl09-subst padl09-subst-tr \
								tr-abstrint ldta08-abstrint \
								ldta09-agidiom \
								hs09-uhcarch \
								theplan

# subtext
TEXT_SUBS					+= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								SharedTypeLang SharedFIOpts \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred SlidesRuler SlidesShuffle SlidesGRIN SlidesStatus SlidesEHCStructure SlidesUHCStructure \
								CodeFragsExplImpl \
								ToolDocShuffle ToolDocRuler ToolDocEHC ToolDocText2Text \
								InternalDocEhcTechnical InternalDocEhcStructure InternalDocBuildSystem \
								TopicGrinBytecode \
								TopicCHRLocalInst \
								TopicEHCStructure \
								TopicUHCArchitecture \
								TopicAbstrInt \
								TopicSubst \
								TopicAGIdiom \
								AppxNotation FrontMatter OldText \
								Poster PosterLDL PosterTrOrPr PosterUHCArch \
								Uniqueness uniqueness/TopicIntroduction uniqueness/TopicEHC \
								uniqueness/TopicNoBindings uniqueness/TopicPolyvariant uniqueness/TopicRecursion uniqueness/TopicPolymorphic uniqueness/TopicParallel uniqueness/TopicDataTypes uniqueness/TopicOverloading \
								uniqueness/TopicBeyondEHC uniqueness/TopicCodeGeneration uniqueness/TopicInspecting \
								uniqueness/TopicPerformance uniqueness/TopicRelatedWork uniqueness/TopicConclusion \
								uniqueness/Slides uniqueness/TopicImplementation \
								HowToDoc HowToExperiment \
								ReleaseHistory \
								RoadMap \
								ThePlan
								

# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism

# GENERIC STYLES TO BE SHARED BY VARIOUS PRODUCTS
# 1	: base (share)
# 9	: slides explimpl: base (share)
# 18: slides: base (share)
# 26: acm paper: base (share)
# 30: book: base (share)
# 32: llncs paper: base (share) - not yet available
# 37: entcs paper: base (share)
# 39: documentation using simplified latex/...: base (share)
# 54: poster: base (share)
# 56: article: base (share)

# ALL PRODUCTS
# 2	: ehc book (previously phd)
# 3	: flops06-ruler, truu-ruler
# 4	: popl07-explimpl, truu-explimpl
# 5	: hw06-impred
# 6	: afp (will be obsolete)
# 7	: scratch (book format)
# 8	: slides afp
# 10: future
# 11: shuffle doc (old, will be obsolete)
# 12: garbage
# 13: poster
# 14: slides ruler
# 15: slides explimpl, general
# 16: slides explimpl, for fpnl dag
# 17: slides overview
# 19: ruler doc
# 20: uniqueness doc
# 21: uniqueness slides
# 22: grin bytecode design
# 23: slides status
# 24: paper "chr & local instances"
# 25: experience report "ehc structure"
# 27: poster LDL
# 28: slides EHC Structure
# 29: ehc doc
# 31: poster Tree Oriented Programming (Siren 2008)
# 33: paper "Abstract Interpretation"
# 35: llvm thesis
# 36: icfp08/padl09 paper on substitution/unification
# 38: ldta09 paper on AG idiom
# 40: doc: shuffle
# 41: doc: how to write doc
# 42: doc: ehc technical (to be started by Jeroen)
# 43: doc: ehc user (to be started by Atze)
# 44: doc: text2text
# 45: doc: how to experiment with ehc
# 46: doc: ehc structure
# 47: doc: getting started
# 48: doc: announcement
# 49: doc: release history
# 50: doc: build system
# 51: paper "UHC Architecture" to be submitted to Haskell Symposium 2009
# 52: doc: roadmap
# 53: slides "UHC Architecture", Haskell Symposium 2009
# 55: poster UHC Architecture (Siren 2009)
# 57: the big plan, i.e. 'vision' + roadmap + directions + projects
# 77: scratch (article format)

TEXT_SHUFFLE_ORDER	+= \
		1 < 2, \
		1 < 3, \
		1 < 4, \
		1 < 5, \
		1 < 6, \
		1 < 8, \
		1 < 10, \
		1 < 11, \
		1 < 18, \
		1 < 19, \
		1 < 20, \
		1 < 22, \
		1 < 26, \
		1 < 30, \
		1 < 77, \
		1 < 37, \
		1 < 54, \
		1 < 56, \
		35, \
		9 < 15, \
		9 < 16, \
		18 < 9, \
		18 < 14, \
		18 < 17, \
		18 < 21, \
		18 < 23, \
		18 < 28, \
		18 < 53, \
		26 < 24, \
		26 < 25, \
		26 < 36, \
		26 < 51, \
		30 < 7, \
		30 < 29, \
		37 < 33, \
		37 < 38, \
		39 < 40, \
		39 < 41, \
		39 < 42, \
		39 < 43, \
		39 < 44, \
		39 < 45, \
		39 < 46, \
		39 < 47, \
		39 < 48, \
		39 < 49, \
		39 < 50, \
		39 < 52, \
		54 < 13, \
		54 < 27, \
		54 < 31, \
		54 < 55, \
		56 < 57
		
