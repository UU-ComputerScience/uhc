# all variants
TEXT_PUB_VARIANTS			+= ruler-doc shuffle-doc ehc-book ehc-doc
TEXT_PRIV_VARIANTS			+= flops06-ruler-paper flops06-ruler \
								popl07-explimpl \
								hw06-impred esop07-impred esop07-impred-tr \
								truu-explimpl truu-ruler \
								phd-paper phd-draft phd-tst phd \
								ehc-book-tst \
								scratch scratch2 \
								poster posterLDL posterTrOrPr \
								slides-ruler slides-ruler-long \
								slides-explimpl slides-explimpl-fpnl slides-overview slides-status slides-ehcstruct \
								gbm \
								uniqueness slides-uniqueness \
								icfp07-chr-locinst icfp07-chr-locinst-blind cc08-chr-locinst \
								icfp07-ehcstruct icfp07-ehcstruct-blind ifl07-ehcstruct

# subtext
TEXT_SUBS					+= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								SharedTypeLang SharedFIOpts \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred SlidesRuler SlidesShuffle SlidesGRIN SlidesStatus SlidesEHCStructure \
								CodeFragsExplImpl \
								ToolDocShuffle ToolDocRuler ToolDocEHC \
								TopicGrinBytecode \
								TopicCHRLocalInst \
								TopicEHCStructure \
								AppxNotation FrontMatter OldText \
								Poster PosterLDL PosterTrOrPr \
								Uniqueness uniqueness/TopicIntroduction uniqueness/TopicEHC \
								uniqueness/TopicNoBindings uniqueness/TopicPolyvariant uniqueness/TopicRecursion uniqueness/TopicPolymorphic uniqueness/TopicParallel uniqueness/TopicDataTypes uniqueness/TopicOverloading \
								uniqueness/TopicBeyondEHC uniqueness/TopicCodeGeneration uniqueness/TopicInspecting \
								uniqueness/TopicPerformance uniqueness/TopicRelatedWork uniqueness/TopicConclusion \
								uniqueness/Slides uniqueness/TopicImplementation

# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism
# 1	: base (share)
# 2	: ehc book (previously phd)
# 3	: flops06-ruler, truu-ruler
# 4	: popl07-explimpl, truu-explimpl
# 5	: hw06-impred
# 6	: afp (will be obsolete)
# 7	: scratch (book format)
# 8	: slides afp
# 9	: slides explimpl: base (share)
# 10: future
# 11: shuffle doc
# 12: garbage
# 13: poster
# 14: slides ruler
# 15: slides explimpl, general
# 16: slides explimpl, for fpnl dag
# 17: slides overview
# 18: slides: base (share)
# 19: ruler doc
# 20: uniqueness doc
# 21: uniqueness slides
# 22: grin bytecode design
# 23: slides status
# 24: paper "chr & local instances"
# 25: experience report "ehc structure"
# 26: acm paper: base (share)
# 27: poster LDL
# 28: slides EHC Structure
# 29: ehc doc
# 30: book: base (share)
# 31: poster TrOrPr
# 77: scratch (article format)

TEXT_SHUFFLE_ORDER			+= 1 < 2, 1 < 3, 1 < 4, 1 < 5, 1 < 6, 30 < 7, 1 < 77, 1 < 8, 18 < 9, 1 < 10, \
								1 < 11, 1 < 13, 18 < 14, 9 < 15, 9 < 16, 18 < 17, 1 < 18, 1 < 19, 1 < 20, \
								18 < 21, 1 < 22, 18 < 23, 26 < 24, 1 < 26, 26 < 25, 1 < 27, 18 < 28, 1 < 30, 30 < 29, 1 < 31
