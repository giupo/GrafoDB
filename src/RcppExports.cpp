// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// do_history_nativo
void do_history_nativo(SEXP username, SEXP password, SEXP hostname, SEXP port, SEXP dbname, SEXP names, SEXP tag, SEXP ordinale);
RcppExport SEXP GrafoDB_do_history_nativo(SEXP usernameSEXP, SEXP passwordSEXP, SEXP hostnameSEXP, SEXP portSEXP, SEXP dbnameSEXP, SEXP namesSEXP, SEXP tagSEXP, SEXP ordinaleSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type username(usernameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type password(passwordSEXP );
        Rcpp::traits::input_parameter< SEXP >::type hostname(hostnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type port(portSEXP );
        Rcpp::traits::input_parameter< SEXP >::type dbname(dbnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type names(namesSEXP );
        Rcpp::traits::input_parameter< SEXP >::type tag(tagSEXP );
        Rcpp::traits::input_parameter< SEXP >::type ordinale(ordinaleSEXP );
        do_history_nativo(username, password, hostname, port, dbname, names, tag, ordinale);
    }
    return R_NilValue;
END_RCPP
}
// load_archi
CharacterMatrix load_archi(SEXP username, SEXP password, SEXP hostname, SEXP port, SEXP dbname, SEXP tag);
RcppExport SEXP GrafoDB_load_archi(SEXP usernameSEXP, SEXP passwordSEXP, SEXP hostnameSEXP, SEXP portSEXP, SEXP dbnameSEXP, SEXP tagSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type username(usernameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type password(passwordSEXP );
        Rcpp::traits::input_parameter< SEXP >::type hostname(hostnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type port(portSEXP );
        Rcpp::traits::input_parameter< SEXP >::type dbname(dbnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type tag(tagSEXP );
        CharacterMatrix __result = load_archi(username, password, hostname, port, dbname, tag);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// load_data_nativo
List load_data_nativo(SEXP username, SEXP password, SEXP hostname, SEXP port, SEXP dbname, SEXP names, SEXP tag);
RcppExport SEXP GrafoDB_load_data_nativo(SEXP usernameSEXP, SEXP passwordSEXP, SEXP hostnameSEXP, SEXP portSEXP, SEXP dbnameSEXP, SEXP namesSEXP, SEXP tagSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type username(usernameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type password(passwordSEXP );
        Rcpp::traits::input_parameter< SEXP >::type hostname(hostnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type port(portSEXP );
        Rcpp::traits::input_parameter< SEXP >::type dbname(dbnameSEXP );
        Rcpp::traits::input_parameter< SEXP >::type names(namesSEXP );
        Rcpp::traits::input_parameter< SEXP >::type tag(tagSEXP );
        List __result = load_data_nativo(username, password, hostname, port, dbname, names, tag);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// save_data
void save_data(List dati, List functions, CharacterMatrix archi, CharacterVector tag, CharacterVector newtag);
RcppExport SEXP GrafoDB_save_data(SEXP datiSEXP, SEXP functionsSEXP, SEXP archiSEXP, SEXP tagSEXP, SEXP newtagSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List >::type dati(datiSEXP );
        Rcpp::traits::input_parameter< List >::type functions(functionsSEXP );
        Rcpp::traits::input_parameter< CharacterMatrix >::type archi(archiSEXP );
        Rcpp::traits::input_parameter< CharacterVector >::type tag(tagSEXP );
        Rcpp::traits::input_parameter< CharacterVector >::type newtag(newtagSEXP );
        save_data(dati, functions, archi, tag, newtag);
    }
    return R_NilValue;
END_RCPP
}
