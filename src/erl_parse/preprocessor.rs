use crate::project::ErlProject;

/// Process Erlang source before parsing
/// -define(Name(...), ...).
/// -if(Bool), -ifdef(Macro), -ifndef(Macro), -undef(Macro), -else, -elif(Bool), -endif
/// -error(Term), -warning(Term) (OTP 19+)
/// ?MODULE, ?MODULE_STRING, ?FILE, ?LINE, ?MACHINE='BEAM', ?FUNCTION_NAME, ?FUNCTION_ARITY,
/// ?OTP_RELEASE (OTP 21+)
/// ??MACRO to stringify the tokens in the macro argument
pub fn preprocess_source(project: &mut ErlProject, src: String) -> String {
    dbg!("Preprocess notimpl");
    return src;
}
