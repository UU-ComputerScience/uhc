# all variants
TEXT_PUB_VARIANTS			+= ruler-doc ehc-book ehc-doc
TEXT_DOC_VARIANTS			+= shuffle-doc howtodoc-doc ehc-technical-doc ehc-user-doc text2text-doc
TEXT_PRIV_VARIANTS			+= flops06-ruler-paper flops06-ruler \
								popl07-explimpl \
								hw06-impred esop07-impred esop07-impred-tr \
								truu-explimpl truu-ruler \
								phd-paper phd-draft phd-tst phd \
								ehc-book-tst \
								scratch scratch2 \
								poster posterLDL posterTrOrPr \
								slides-ruler slides-ruler-long \
								slides-explimpl slides-explimpl-fpnl slides-overview slides-status \
								slides-ehcstruct slides-ehcstruct-ufmg \
								gbm \
								uniqueness slides-uniqueness \
								icfp07-chr-locinst icfp07-chr-locinst-blind cc08-chr-locinst \
								icfp07-ehcstruct icfp07-ehcstruct-blind ifl07-ehcstruct \
								icfp08-subst padl09-subst padl09-subst-tr \
								tr-abstrint ldta08-abstrint \
								ldta09-agidiom

# subtext
TEXT_SUBS					+= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								SharedTypeLang SharedFIOpts \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred SlidesRuler SlidesShuffle SlidesGRIN SlidesStatus SlidesEHCStructure \
								CodeFragsExplImpl \
								ToolDocShuffle ToolDocRuler ToolDocEHC ToolDocText2Text \
								InternalDocEHC \
								TopicGrinBytecode \
								TopicCHRLocalInst \
								TopicEHCStructure \
								TopicAbstrInt \
								TopicSubst \
								TopicAGIdiom \
								AppxNotation FrontMatter OldText \
								Poster PosterLDL PosterTrOrPr \
								Uniqueness uniqueness/TopicIntroduction uniqueness/TopicEHC \
								uniqueness/TopicNoBindings uniqueness/TopicPolyvariant uniqueness/TopicRecursion uniqueness/TopicPolymorphic uniqueness/TopicParallel uniqueness/TopicDataTypes uniqueness/TopicOverloading \
								uniqueness/TopicBeyondEHC uniqueness/TopicCodeGeneration uniqueness/TopicInspecting \
								uniqueness/TopicPerformance uniqueness/TopicRelatedWork uniqueness/TopicConclusion \
								uniqueness/Slides uniqueness/TopicImplementation \
								DocDoc
								

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
# 40: shuffle doc
# 41: how to write doc doc (to be done by Atze)
# 42: ehc technical doc (to be started by Jeroen)
# 43: ehc user doc (to be started by Atze)
# 44: text2text doc (to be done by Atze)
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
		1 < 13, \
		1 < 18, \
		1 < 19, \
		1 < 20, \
		1 < 22, \
		1 < 26, \
		1 < 27, \
		1 < 30, \
		1 < 31, \
		1 < 77, \
		1 < 37, \
		35, \
		9 < 15, \
		9 < 16, \
		18 < 9, \
		18 < 14, \
		18 < 17, \
		18 < 21, \
		18 < 23, \
		18 < 28, \
		26 < 24, \
		26 < 25, \
		26 < 36, \
		30 < 7, \
		30 < 29, \
		37 < 33, \
		37 < 38, \
		39 < 40, \
		39 < 41, \
		39 < 42, \
		39 < 43, \
		39 < 44
		
