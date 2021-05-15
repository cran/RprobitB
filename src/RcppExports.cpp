// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// start_timer
void start_timer();
RcppExport SEXP _RprobitB_start_timer() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    start_timer();
    return R_NilValue;
END_RCPP
}
// update_timer
void update_timer(int rep, int R);
RcppExport SEXP _RprobitB_update_timer(SEXP repSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type rep(repSEXP);
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    update_timer(rep, R);
    return R_NilValue;
END_RCPP
}
// end_timer
void end_timer(int R);
RcppExport SEXP _RprobitB_end_timer(SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    end_timer(R);
    return R_NilValue;
END_RCPP
}
// dmvnrm_arma_mc
arma::vec dmvnrm_arma_mc(arma::mat const& x, arma::vec const& mean, arma::mat const& sigma, bool const logd);
RcppExport SEXP _RprobitB_dmvnrm_arma_mc(SEXP xSEXP, SEXP meanSEXP, SEXP sigmaSEXP, SEXP logdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat const& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool const >::type logd(logdSEXP);
    rcpp_result_gen = Rcpp::wrap(dmvnrm_arma_mc(x, mean, sigma, logd));
    return rcpp_result_gen;
END_RCPP
}
// rdirichlet
arma::vec rdirichlet(arma::vec alpha);
RcppExport SEXP _RprobitB_rdirichlet(SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(rdirichlet(alpha));
    return rcpp_result_gen;
END_RCPP
}
// rwishart
List rwishart(double nu, arma::mat const& V);
RcppExport SEXP _RprobitB_rwishart(SEXP nuSEXP, SEXP VSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type V(VSEXP);
    rcpp_result_gen = Rcpp::wrap(rwishart(nu, V));
    return rcpp_result_gen;
END_RCPP
}
// gibbs_loop
List gibbs_loop(int R, int B, int nprint, int N, int Jm1, int P_f, int P_r, int C, List lcus, List suff_statistics, List prior, List init);
RcppExport SEXP _RprobitB_gibbs_loop(SEXP RSEXP, SEXP BSEXP, SEXP nprintSEXP, SEXP NSEXP, SEXP Jm1SEXP, SEXP P_fSEXP, SEXP P_rSEXP, SEXP CSEXP, SEXP lcusSEXP, SEXP suff_statisticsSEXP, SEXP priorSEXP, SEXP initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< int >::type nprint(nprintSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type Jm1(Jm1SEXP);
    Rcpp::traits::input_parameter< int >::type P_f(P_fSEXP);
    Rcpp::traits::input_parameter< int >::type P_r(P_rSEXP);
    Rcpp::traits::input_parameter< int >::type C(CSEXP);
    Rcpp::traits::input_parameter< List >::type lcus(lcusSEXP);
    Rcpp::traits::input_parameter< List >::type suff_statistics(suff_statisticsSEXP);
    Rcpp::traits::input_parameter< List >::type prior(priorSEXP);
    Rcpp::traits::input_parameter< List >::type init(initSEXP);
    rcpp_result_gen = Rcpp::wrap(gibbs_loop(R, B, nprint, N, Jm1, P_f, P_r, C, lcus, suff_statistics, prior, init));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RprobitB_start_timer", (DL_FUNC) &_RprobitB_start_timer, 0},
    {"_RprobitB_update_timer", (DL_FUNC) &_RprobitB_update_timer, 2},
    {"_RprobitB_end_timer", (DL_FUNC) &_RprobitB_end_timer, 1},
    {"_RprobitB_dmvnrm_arma_mc", (DL_FUNC) &_RprobitB_dmvnrm_arma_mc, 4},
    {"_RprobitB_rdirichlet", (DL_FUNC) &_RprobitB_rdirichlet, 1},
    {"_RprobitB_rwishart", (DL_FUNC) &_RprobitB_rwishart, 2},
    {"_RprobitB_gibbs_loop", (DL_FUNC) &_RprobitB_gibbs_loop, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_RprobitB(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
