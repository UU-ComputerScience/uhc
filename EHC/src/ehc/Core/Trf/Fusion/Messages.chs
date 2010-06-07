%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.Messages}
%%]


%%[(8 codegen)


-- alg/Inline.lhs

inlineTerm_Variable_Not_Found = "inlineTermS: Couldn't find the variable."
termS_Not_Expected_Form = "The TermS is not an expected form."
constructor_withOut_Variables = "A constructor has been detected without an empty variable associated with it."
definition_Fault = "Fault in the function's definition"
reading_Fault = "Error while loading the function"

-- interprete/SystemInterface.lhs

functions_Not_Found nombre1 nombre2 = ("The functions " ++ (show nombre1) ++ " and " ++ (show nombre2) ++ "  couldn't be found.")
function_Not_Found nombre1 = "The function " ++ (show nombre1) ++ " couldn't be found."
not_Found nombre1 = (show nombre1) ++ " couldn't be found."
file_Not_Found filename = "The file " ++ (show filename) ++ " couldn't be found."
function_Not_Found_Either_File_AND_System nombre1 = "The function " ++ (show nombre1) ++ " couldn't be found neither in the file nor the system."			
function_Not_Found_In_File nombre1 = "The function " ++ (show nombre1) ++ " couldn't be found in the system."			
function_Not_Found_In_System nombre1 = "The function " ++ (show nombre1) ++ "  couldn't be found in the system."			
couldnt_Load = "Couldn't load the file."
comparation = "COMPARISON"
both_Defs_Are_Same_Function = "Both definitions corresponds to the same function."	 
defintions_Are_Diferent_Functions name inlineMon inlineFile = "The definition of " ++ (show name) ++ " in the file: \n\n " ++ (show inlineMon) ++ "\n and in the file the " ++ (show name) ++ " is defined as follows: \n\n " ++ (show inlineFile)			 
couldnt_Load_From_File filename = "Could not load the definitions from the file: " ++ filename					
not_Found_Specified_Functions = "Some of those function couldn't be found."
directory_Not_Found directorio = "The directory " ++ directorio ++ " couldn't be found."
unknown_Error_While_Loading archivo = "Unknow error while loading the file: " ++  archivo ++ "."
comparation_All_Functinos = "COMPARISON OF THE FRAMEWORK FUNCTIONS"
everyDef_Is_Ok = "Every definition from the framework match with the definition recorded in the files.\n"
the_Diff_Defs_Are_Listed_Below = "The functions which are has different definitions are the following:"
error_Trying_Load_Hylo def = "Error while loading hylomorphism = " ++ (show def)
ok_Comparison_Label = "OK  "			
err_Comparison_Label = "ERR_FUS "		  

-- alg/FuseFace.lhs

left_Hylo_Not_Sigma_Form = "Leftmost hylomorphism is not in Sigma form, nor InF."
right_Hylo_Not_Tau_Form = "Rightmost hylomorphism is not in Tau form, nor OutF."		      
fuseOperation_Label ="fuse"
fuse_Tau_Operation_Label = "Taufuse"
first_Hylo_Not_OutF_Form = "The first hylomorphism isn't in a OutF's form."
second_Hylo_Not_Phi_Form = "The second hylomorphism isn't in a generic's form."
fuse_Sigma_Operation_Label = "Sigmafuse"		      
first_Hylo_Not_Psi_Form = "The first hylomorphism isn't in a generic's form."
second_Hylo_Not_InF_Form = "The second hylomorphism isn't in InF's form."		     
couldnt_Fuse_Hylos = "Couldn't fuse the hylomorphisms."

-- alg/FunctorRep.lhs

coalgebra_Should_Not_Return_Terms_Diffrent_From_Vars = "getCata: The coalgebra shouldn't return non-variable terms."
not_Recibed_OutF = "etaPara: Didn't recived OutF."						  
var_Rec_Not_Binded = " A recursive variable is not bound"	      
tWeta_Unexpected = "Unexpected TWeta founded."
twaComp_Unexpected = " Unexpected TWacomp founded."
twBottom_Unexpected = " Unexpected TWbottom founded."
getCata_Label = "getCata"
unexpected_Constructed_Pair = " Expected pair."
unexpected_Non_Variables = "Unexpected non-variables."			 
unexpected_Pattern p = " Pattern didn't expected  "++ show p
variable_Not_Found vt = " Input variable not found  "++show vt		  

-- lib/Utils.lhs

not_Defined_For_Applied_Pattern p = " Not defined for the applied pattern:"++show p
not_Defined_For_Applied_Term t = " Not defined for the applied term:"++show t


-- lib/HsPretty.lhs

infix_Operator_Without_Characters_In_Name = "Infix operator without a name"
infix_Constructor_Without_Characters_In_Name = "Infix constructor without a name"				       
display_Not_Define_For_Term = "It is not defined the way to show the term."					  


-- alg/HyloRep

no_Sound_Operation_Aplication = "It has no sense to invoke the requested operation."

-- interprete/Sistema.hs

could_not_read_any_input = "We could not read any input. Chances are that the file is empty,\n"++
                           "does not exists or you don't have enough privileges to read it."
problem_Label = "Problem: "
nonCommand_Label = "Not a command."
error_Found_In_Grammar m line = "Error found in the grammar, " ++ m ++ "\n" ++ line	      
bad_File_Form = "The file is not in the proper format."		      
not_Tau_Derived_For_Function = "The requested function doesn't have the Tau derived."	   
not_Sigma_Derived_For_Function = "The requested function doesn't have the Sigma derivated."			  
here_We_Display_All_Defs = "This is the list of definitions in the environment"
finish_OK_Label = "Successfully terminated."		      
error_Exception ctx = "An error exception detected:" ++ ctx	     
welcmssg = "Welcome to the Interactive Fusion System."
homedir = "/inco/group02/fusion/usr/mgiorgi/src"
load_command   = "load fn           load definitions form file fn"
cmd_command    = "!cmd              executes de cmd command"
save_command   = "save f1..fn       saves all files f1..fn"
hylo_command   = "hn:               dispplays the definition of hn as hylomorphism"
envi_command   = "env               displays the list of definitions in the environment"
def_command    = "hn                displays the recursive definition of hn"
fuse_command   = "nh = h1 . h2      fuse h1.h2 & binds the result to the identifier nh"
help_command   = "help              shows the available commands"  
check_command  = "checkfile fn      matches the definitions of previous fusions calculations in a repository"  
assertEq_command= "assertEq f g      prints an error messages if the definitions are different (modulus alpha conversion)"  
cata_command   = "cata h            displays h as catamorphism"  
ana_command    = "ana h             displays h as anamorphism"  
quit_command   = "quit              ends the program"


-- /parser/HsLexer

iError_Empty_Context = "Internal error: empty context in lexToken"
iError_Empty_Input_In_LexToken = "Internal error: empty input in lexToken"			  
iError_lexChar = "Internal error: lexChar"				    
illegal_Float = "Illegal float"	    
illegal_Character c = "illegal character \'" ++ show c ++ "\'\n"	   
improperly_Terminated_Char_Const = "Improperly terminated character constant"		 
improperly_Terminated_Str = "Improperly terminated string"			      
illegal_Char_Str_Gap = "Illegal character in string gap"		       
iError_stringGap = "Internal error: stringGap"		  
illegal_Ctrl_Char = "Illegal control character"	      
iError_nestedComment = "Internal error: nestedComment"	       


-- /parser/Env.hs

isNot_Env n = "getAllDefEnv: " ++ n ++ " is not in environment"

-- /parser/HsParseMonad.hs

iError_Empty_Context_In_PopContext = "Internal error: empty context in popContext"
error_Label = "ERROR: "				

-- /interprete/Exception.hs

cannot_Find_Label x = "cannot find " ++ show x

-- /alg/SystemMonDef.lhs

not_Satured t = "The term (" ++ (show t) ++ ") is a recursive call (possibly mutual) which changes\n"++
                "an argument other than the last one.\n"++
                "Only the last recursive argument may differ from that of the initial invocation.\n"++
                "Is there any way you could rewrite the definition to meet this restriction?"

--                  "It also may be the case that equations of a definition where typed giving \n"++
--                  "different names to constant arguments in some of them.\n"++
--                  "Example:\n\n"++
--                  "filter p [] = ... \n"++
--                  "filter q (a:as) = ... \n"++
--                  "Please rewrite the equations so they use the same names for the same\n constant arguments.\n\n"++
--                  "Example:\n"++
--                  "filter p [] = ... \n"++
--                  "filter p (a:as) = ... \n"

not_Expected t = "Not expected: (" ++ (show t) ++ ")."
not_Derivable = "Not derivable."
not_Legal_Term = "Not legal term."
not_InF_Term = "The algebra of the hylomorphism is not InF."
not_OutF_Term = "The coalgebra of the hylomorphism is not OutF."
unmatched_F_Error = "The funtors do not match"
debug_Message t chrs = "Debug message for the term " ++ (show t) ++ ":" ++ chrs
error_Message chrs = chrs

%%]
