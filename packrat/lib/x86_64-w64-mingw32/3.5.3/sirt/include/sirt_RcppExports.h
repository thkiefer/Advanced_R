// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_sirt_RCPPEXPORTS_H_GEN_
#define RCPP_sirt_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace sirt {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("sirt", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("sirt", "_sirt_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in sirt");
            }
        }
    }

    inline Rcpp::NumericMatrix sirt_rcpp_invariance_alignment_lambda_transformed(Rcpp::NumericMatrix lambda, Rcpp::NumericVector psi0) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_lambda_transformed)(SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_lambda_transformed p_sirt_rcpp_invariance_alignment_lambda_transformed = NULL;
        if (p_sirt_rcpp_invariance_alignment_lambda_transformed == NULL) {
            validateSignature("Rcpp::NumericMatrix(*sirt_rcpp_invariance_alignment_lambda_transformed)(Rcpp::NumericMatrix,Rcpp::NumericVector)");
            p_sirt_rcpp_invariance_alignment_lambda_transformed = (Ptr_sirt_rcpp_invariance_alignment_lambda_transformed)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_lambda_transformed");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_lambda_transformed(Shield<SEXP>(Rcpp::wrap(lambda)), Shield<SEXP>(Rcpp::wrap(psi0)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericMatrix >(rcpp_result_gen);
    }

    inline Rcpp::NumericMatrix sirt_rcpp_invariance_alignment_nu_transformed(Rcpp::NumericMatrix nu, Rcpp::NumericMatrix lambda, Rcpp::NumericVector alpha0, Rcpp::NumericVector psi0) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_nu_transformed)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_nu_transformed p_sirt_rcpp_invariance_alignment_nu_transformed = NULL;
        if (p_sirt_rcpp_invariance_alignment_nu_transformed == NULL) {
            validateSignature("Rcpp::NumericMatrix(*sirt_rcpp_invariance_alignment_nu_transformed)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector)");
            p_sirt_rcpp_invariance_alignment_nu_transformed = (Ptr_sirt_rcpp_invariance_alignment_nu_transformed)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_nu_transformed");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_nu_transformed(Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(lambda)), Shield<SEXP>(Rcpp::wrap(alpha0)), Shield<SEXP>(Rcpp::wrap(psi0)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericMatrix >(rcpp_result_gen);
    }

    inline double sirt_rcpp_invariance_alignment_simplicity_function_value(Rcpp::CharacterVector type, double parm1, double parm2, double scale, double power, double eps) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_simplicity_function_value)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_simplicity_function_value p_sirt_rcpp_invariance_alignment_simplicity_function_value = NULL;
        if (p_sirt_rcpp_invariance_alignment_simplicity_function_value == NULL) {
            validateSignature("double(*sirt_rcpp_invariance_alignment_simplicity_function_value)(Rcpp::CharacterVector,double,double,double,double,double)");
            p_sirt_rcpp_invariance_alignment_simplicity_function_value = (Ptr_sirt_rcpp_invariance_alignment_simplicity_function_value)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_simplicity_function_value");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_simplicity_function_value(Shield<SEXP>(Rcpp::wrap(type)), Shield<SEXP>(Rcpp::wrap(parm1)), Shield<SEXP>(Rcpp::wrap(parm2)), Shield<SEXP>(Rcpp::wrap(scale)), Shield<SEXP>(Rcpp::wrap(power)), Shield<SEXP>(Rcpp::wrap(eps)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline Rcpp::List sirt_rcpp_invariance_alignment_opt_fct(Rcpp::NumericMatrix nu, Rcpp::NumericMatrix lambda, Rcpp::NumericVector alpha0, Rcpp::NumericVector psi0, Rcpp::IntegerMatrix group_combis, Rcpp::NumericMatrix wgt, Rcpp::NumericVector align_scale, Rcpp::NumericVector align_pow, double eps, Rcpp::NumericMatrix wgt_combi, Rcpp::CharacterVector type) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_opt_fct)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_opt_fct p_sirt_rcpp_invariance_alignment_opt_fct = NULL;
        if (p_sirt_rcpp_invariance_alignment_opt_fct == NULL) {
            validateSignature("Rcpp::List(*sirt_rcpp_invariance_alignment_opt_fct)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::IntegerMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,double,Rcpp::NumericMatrix,Rcpp::CharacterVector)");
            p_sirt_rcpp_invariance_alignment_opt_fct = (Ptr_sirt_rcpp_invariance_alignment_opt_fct)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_opt_fct");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_opt_fct(Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(lambda)), Shield<SEXP>(Rcpp::wrap(alpha0)), Shield<SEXP>(Rcpp::wrap(psi0)), Shield<SEXP>(Rcpp::wrap(group_combis)), Shield<SEXP>(Rcpp::wrap(wgt)), Shield<SEXP>(Rcpp::wrap(align_scale)), Shield<SEXP>(Rcpp::wrap(align_pow)), Shield<SEXP>(Rcpp::wrap(eps)), Shield<SEXP>(Rcpp::wrap(wgt_combi)), Shield<SEXP>(Rcpp::wrap(type)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline double sirt_rcpp_invariance_alignment_simplicity_function_gradient(Rcpp::CharacterVector type, double parm1, double parm2, double scale, double power, double eps) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_simplicity_function_gradient)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_simplicity_function_gradient p_sirt_rcpp_invariance_alignment_simplicity_function_gradient = NULL;
        if (p_sirt_rcpp_invariance_alignment_simplicity_function_gradient == NULL) {
            validateSignature("double(*sirt_rcpp_invariance_alignment_simplicity_function_gradient)(Rcpp::CharacterVector,double,double,double,double,double)");
            p_sirt_rcpp_invariance_alignment_simplicity_function_gradient = (Ptr_sirt_rcpp_invariance_alignment_simplicity_function_gradient)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_simplicity_function_gradient");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_simplicity_function_gradient(Shield<SEXP>(Rcpp::wrap(type)), Shield<SEXP>(Rcpp::wrap(parm1)), Shield<SEXP>(Rcpp::wrap(parm2)), Shield<SEXP>(Rcpp::wrap(scale)), Shield<SEXP>(Rcpp::wrap(power)), Shield<SEXP>(Rcpp::wrap(eps)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector sirt_rcpp_invariance_alignment_opt_grad(Rcpp::NumericMatrix nu, Rcpp::NumericMatrix lambda, Rcpp::NumericVector alpha0, Rcpp::NumericVector psi0, Rcpp::IntegerMatrix group_combis, Rcpp::NumericMatrix wgt, Rcpp::NumericVector align_scale, Rcpp::NumericVector align_pow, double eps, Rcpp::NumericMatrix wgt_combi, Rcpp::CharacterVector type) {
        typedef SEXP(*Ptr_sirt_rcpp_invariance_alignment_opt_grad)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_invariance_alignment_opt_grad p_sirt_rcpp_invariance_alignment_opt_grad = NULL;
        if (p_sirt_rcpp_invariance_alignment_opt_grad == NULL) {
            validateSignature("Rcpp::NumericVector(*sirt_rcpp_invariance_alignment_opt_grad)(Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::IntegerMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,double,Rcpp::NumericMatrix,Rcpp::CharacterVector)");
            p_sirt_rcpp_invariance_alignment_opt_grad = (Ptr_sirt_rcpp_invariance_alignment_opt_grad)R_GetCCallable("sirt", "_sirt_sirt_rcpp_invariance_alignment_opt_grad");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_invariance_alignment_opt_grad(Shield<SEXP>(Rcpp::wrap(nu)), Shield<SEXP>(Rcpp::wrap(lambda)), Shield<SEXP>(Rcpp::wrap(alpha0)), Shield<SEXP>(Rcpp::wrap(psi0)), Shield<SEXP>(Rcpp::wrap(group_combis)), Shield<SEXP>(Rcpp::wrap(wgt)), Shield<SEXP>(Rcpp::wrap(align_scale)), Shield<SEXP>(Rcpp::wrap(align_pow)), Shield<SEXP>(Rcpp::wrap(eps)), Shield<SEXP>(Rcpp::wrap(wgt_combi)), Shield<SEXP>(Rcpp::wrap(type)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline double sirt_rcpp_linking_haebara_fct_optim_one_item(Rcpp::NumericVector theta, Rcpp::NumericVector prob_theta, Rcpp::NumericMatrix aM, Rcpp::NumericMatrix bM, Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, int ii, int ss, Rcpp::CharacterVector dist, double eps) {
        typedef SEXP(*Ptr_sirt_rcpp_linking_haebara_fct_optim_one_item)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_linking_haebara_fct_optim_one_item p_sirt_rcpp_linking_haebara_fct_optim_one_item = NULL;
        if (p_sirt_rcpp_linking_haebara_fct_optim_one_item == NULL) {
            validateSignature("double(*sirt_rcpp_linking_haebara_fct_optim_one_item)(Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,int,int,Rcpp::CharacterVector,double)");
            p_sirt_rcpp_linking_haebara_fct_optim_one_item = (Ptr_sirt_rcpp_linking_haebara_fct_optim_one_item)R_GetCCallable("sirt", "_sirt_sirt_rcpp_linking_haebara_fct_optim_one_item");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_linking_haebara_fct_optim_one_item(Shield<SEXP>(Rcpp::wrap(theta)), Shield<SEXP>(Rcpp::wrap(prob_theta)), Shield<SEXP>(Rcpp::wrap(aM)), Shield<SEXP>(Rcpp::wrap(bM)), Shield<SEXP>(Rcpp::wrap(a)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(mu)), Shield<SEXP>(Rcpp::wrap(sigma)), Shield<SEXP>(Rcpp::wrap(ii)), Shield<SEXP>(Rcpp::wrap(ss)), Shield<SEXP>(Rcpp::wrap(dist)), Shield<SEXP>(Rcpp::wrap(eps)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline double sirt_rcpp_linking_haebara_fct_optim(int NI, int NS, Rcpp::CharacterVector dist, Rcpp::NumericMatrix aM, Rcpp::NumericMatrix bM, Rcpp::NumericVector theta, Rcpp::NumericVector prob_theta, Rcpp::LogicalMatrix est_pars, Rcpp::NumericMatrix wgtM, Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, double eps) {
        typedef SEXP(*Ptr_sirt_rcpp_linking_haebara_fct_optim)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_linking_haebara_fct_optim p_sirt_rcpp_linking_haebara_fct_optim = NULL;
        if (p_sirt_rcpp_linking_haebara_fct_optim == NULL) {
            validateSignature("double(*sirt_rcpp_linking_haebara_fct_optim)(int,int,Rcpp::CharacterVector,Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::LogicalMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,double)");
            p_sirt_rcpp_linking_haebara_fct_optim = (Ptr_sirt_rcpp_linking_haebara_fct_optim)R_GetCCallable("sirt", "_sirt_sirt_rcpp_linking_haebara_fct_optim");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_linking_haebara_fct_optim(Shield<SEXP>(Rcpp::wrap(NI)), Shield<SEXP>(Rcpp::wrap(NS)), Shield<SEXP>(Rcpp::wrap(dist)), Shield<SEXP>(Rcpp::wrap(aM)), Shield<SEXP>(Rcpp::wrap(bM)), Shield<SEXP>(Rcpp::wrap(theta)), Shield<SEXP>(Rcpp::wrap(prob_theta)), Shield<SEXP>(Rcpp::wrap(est_pars)), Shield<SEXP>(Rcpp::wrap(wgtM)), Shield<SEXP>(Rcpp::wrap(a)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(mu)), Shield<SEXP>(Rcpp::wrap(sigma)), Shield<SEXP>(Rcpp::wrap(eps)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<double >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector sirt_rcpp_linking_haebara_grad_optim_one_item(Rcpp::NumericVector theta, Rcpp::NumericVector prob_theta, Rcpp::NumericMatrix aM, Rcpp::NumericMatrix bM, Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, int ii, int ss, Rcpp::CharacterVector dist, double eps, int NI, int NS, Rcpp::IntegerVector index_a, Rcpp::IntegerVector index_b, Rcpp::IntegerVector index_mu, Rcpp::IntegerVector index_sigma, Rcpp::NumericMatrix wgtM, Rcpp::NumericVector grad0) {
        typedef SEXP(*Ptr_sirt_rcpp_linking_haebara_grad_optim_one_item)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_linking_haebara_grad_optim_one_item p_sirt_rcpp_linking_haebara_grad_optim_one_item = NULL;
        if (p_sirt_rcpp_linking_haebara_grad_optim_one_item == NULL) {
            validateSignature("Rcpp::NumericVector(*sirt_rcpp_linking_haebara_grad_optim_one_item)(Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,int,int,Rcpp::CharacterVector,double,int,int,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::NumericMatrix,Rcpp::NumericVector)");
            p_sirt_rcpp_linking_haebara_grad_optim_one_item = (Ptr_sirt_rcpp_linking_haebara_grad_optim_one_item)R_GetCCallable("sirt", "_sirt_sirt_rcpp_linking_haebara_grad_optim_one_item");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_linking_haebara_grad_optim_one_item(Shield<SEXP>(Rcpp::wrap(theta)), Shield<SEXP>(Rcpp::wrap(prob_theta)), Shield<SEXP>(Rcpp::wrap(aM)), Shield<SEXP>(Rcpp::wrap(bM)), Shield<SEXP>(Rcpp::wrap(a)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(mu)), Shield<SEXP>(Rcpp::wrap(sigma)), Shield<SEXP>(Rcpp::wrap(ii)), Shield<SEXP>(Rcpp::wrap(ss)), Shield<SEXP>(Rcpp::wrap(dist)), Shield<SEXP>(Rcpp::wrap(eps)), Shield<SEXP>(Rcpp::wrap(NI)), Shield<SEXP>(Rcpp::wrap(NS)), Shield<SEXP>(Rcpp::wrap(index_a)), Shield<SEXP>(Rcpp::wrap(index_b)), Shield<SEXP>(Rcpp::wrap(index_mu)), Shield<SEXP>(Rcpp::wrap(index_sigma)), Shield<SEXP>(Rcpp::wrap(wgtM)), Shield<SEXP>(Rcpp::wrap(grad0)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

    inline Rcpp::NumericVector sirt_rcpp_linking_haebara_grad_optim(int NI, int NS, Rcpp::CharacterVector dist, Rcpp::NumericMatrix aM, Rcpp::NumericMatrix bM, Rcpp::NumericVector theta, Rcpp::NumericVector prob_theta, Rcpp::LogicalMatrix est_pars, Rcpp::NumericMatrix wgtM, Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, double eps, Rcpp::IntegerVector index_a, Rcpp::IntegerVector index_b, Rcpp::IntegerVector index_mu, Rcpp::IntegerVector index_sigma, Rcpp::CharacterVector parnames, int NP) {
        typedef SEXP(*Ptr_sirt_rcpp_linking_haebara_grad_optim)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sirt_rcpp_linking_haebara_grad_optim p_sirt_rcpp_linking_haebara_grad_optim = NULL;
        if (p_sirt_rcpp_linking_haebara_grad_optim == NULL) {
            validateSignature("Rcpp::NumericVector(*sirt_rcpp_linking_haebara_grad_optim)(int,int,Rcpp::CharacterVector,Rcpp::NumericMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::LogicalMatrix,Rcpp::NumericMatrix,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,Rcpp::NumericVector,double,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::CharacterVector,int)");
            p_sirt_rcpp_linking_haebara_grad_optim = (Ptr_sirt_rcpp_linking_haebara_grad_optim)R_GetCCallable("sirt", "_sirt_sirt_rcpp_linking_haebara_grad_optim");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sirt_rcpp_linking_haebara_grad_optim(Shield<SEXP>(Rcpp::wrap(NI)), Shield<SEXP>(Rcpp::wrap(NS)), Shield<SEXP>(Rcpp::wrap(dist)), Shield<SEXP>(Rcpp::wrap(aM)), Shield<SEXP>(Rcpp::wrap(bM)), Shield<SEXP>(Rcpp::wrap(theta)), Shield<SEXP>(Rcpp::wrap(prob_theta)), Shield<SEXP>(Rcpp::wrap(est_pars)), Shield<SEXP>(Rcpp::wrap(wgtM)), Shield<SEXP>(Rcpp::wrap(a)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(mu)), Shield<SEXP>(Rcpp::wrap(sigma)), Shield<SEXP>(Rcpp::wrap(eps)), Shield<SEXP>(Rcpp::wrap(index_a)), Shield<SEXP>(Rcpp::wrap(index_b)), Shield<SEXP>(Rcpp::wrap(index_mu)), Shield<SEXP>(Rcpp::wrap(index_sigma)), Shield<SEXP>(Rcpp::wrap(parnames)), Shield<SEXP>(Rcpp::wrap(NP)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::NumericVector >(rcpp_result_gen);
    }

}

#endif // RCPP_sirt_RCPPEXPORTS_H_GEN_