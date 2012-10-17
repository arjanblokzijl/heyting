package heyting
package typing

import basictypes._
import Printing._
import basictypes.Types._
import ast._
import prettyprint.Doc
import prettyprint.Docs._
import Tc._

sealed trait Expected[A]
case class Infer[A](aref: Ref[A]) extends Expected[A]
case class Check[A](a: A) extends Expected[A]

trait TypeCheckFunctions {

  private final def litType(l: Literal): Sigma = l match {
    case IntLit(_) => IntT
    case StringLit(_) => StringT
    case CharLit(_) => CharT
    case DoubleLit(_) => DoubleT
  }

  def tcExpr(e: Term): Tc[Term] =
    inferSigma(e).flatMap(e2 => e2.tpe match {
      case Some(t) => zonkType(t).map(e.typed)
      case None => failTc(text("panic: No type found after inferring sigma"))
    })

  final def checkRho(expr: Term, ty: Rho): Tc[Term] = tcRho(expr, Check(ty))

  /**
   * Type check a Rho term.
   * Invariant: if the second argument is (Check rho),
   * then rho is in weak-prenex form
   */
  def tcRho(t: Term, e: Expected[Rho]): Tc[Term] = {
    (t, e) match {
      case (Lit(l, id), exp_ty) => instSigma(t, litType(l), exp_ty)
      case (Var(v), exp_ty) => for {
        v_sigma <- lookupVar(v)
        sig <- instSigma(t, v_sigma, exp_ty)
      } yield sig
      case (App(fun, arg, id), exp_ty) => for {
        fun_ty <- inferRho(fun)
        (arg_ty, res_ty) <- unifyFun(fun_ty)
        _ <- checkSigma(arg, arg_ty)
        sig <- instSigma(t, res_ty, exp_ty)
      } yield sig
      case (Lam(v, body, id), Check(exp_ty)) => for {
        (var_ty, body_ty) <- unifyFun(exp_ty)
        rho <- (checkRho(body, body_ty)).extendVarEnv(v, var_ty)
      } yield rho
      case (Lam(v, body, id), Infer(ref)) => for {
        var_ty <- newTyVarTy
        body_ty <- inferRho(body).extendVarEnv(v, var_ty)
        rho <- writeTcRef(ref, var_ty --> body_ty).map(t.typed)
      } yield rho
      case (ALam(v, var_ty, body, id), Check(exp_ty)) => for {
        (arg_ty, body_ty) <- unifyFun(exp_ty)
        _ <- subsCheck(arg_ty, var_ty)
        rho <- checkRho(body, body_ty).extendVarEnv(v, var_ty)
      } yield rho
      case (ALam(v, var_ty, body, id), Infer(ref)) => for {
        body_ty <- inferRho(body).extendVarEnv(v, var_ty)
        rho <- writeTcRef(ref, var_ty --> body_ty)
//        res <- t.typed(rho)
      } yield t.typed(rho)
      case (Let(v, rhs), Infer(ref)) => for {
        var_ty <- newTyVarTy
        body_ty <- inferRho(rhs).extendVarEnv(v, var_ty)
        rho <- writeTcRef(ref, body_ty)
//        exp <- t.typed(rho)
      } yield t.typed(rho)
      case (Let(v, rhs), exp_ty) => for {
        var_ty <- newTyVarTy
        rho <- tcRho(rhs, exp_ty).extendVarEnv(v, var_ty)
      } yield rho
      case (Ann(body, ann_ty, id), exp_ty) => for {
        _ <- checkSigma(body, ann_ty)
        sig <- instSigma(t, ann_ty, exp_ty)
      } yield sig
    }
  }

  def inferRho(expr: Term): Tc[Rho] = {
    for {
      ref <- newTcRef[Rho](null.asInstanceOf[Rho])
      _ <- tcRho(expr, Infer(ref))
      res <- readTcRef(ref)
    } yield res
  }

  /**Subsumption check for Rho types. Invariant: the second argument is in weak-prenex form.*/
  def subsCheckRho(s: Sigma, r: Rho): Tc[Rho] = (s, r) match {
    case (sigma1@ForAll(_, _), rho2) => for {
      rho1 <- instantiate(sigma1)
      rho <- subsCheckRho(rho1, rho2)
    } yield rho
    case (rho1, Fun(a2, r2)) => for {
      (a1, r1) <- unifyFun(rho1)
      rho <- subsCheckFun(a1, r1, a2, r2)
    } yield rho
    case (Fun(a1, r1), rho2) => for {
      (a2, r2) <- unifyFun(rho2)
      rho <- subsCheckFun(a1, r1, a2, r2)
    } yield rho
    case (tau1, tau2) => unify(tau1, tau1) //revert to ordinary unification
  }

  /**
   * `subsCheck args off exp` checks that
   * `off` is at least as polymorphic as `args -> exp`
   */
  def subsCheck(sigma1: Sigma, sigma2: Sigma): Tc[Unit] = {
    def bad_tvs(tys: Seq[Type], esc_tvs: Seq[Type]): Seq[Type] =
      tys.filter(esc_tvs.contains)
    for {
      (skol_tvs, rho2) <- skolemise(sigma2)
      esc_tvs <- getFreeTyVars(Vector(sigma1, sigma2))
    } yield {
      val bad = bad_tvs(skol_tvs, esc_tvs)
      check(bad.isEmpty,
            vcat(Vector(text("Subsumption check failed:"),
                        nest(2, typeOutput.ppr(sigma1)),
                        text("is not as polymorphic as"),
                        nest(2, typeOutput.ppr(sigma2)))))
    }
  }

  def subsCheckFun(a1: Sigma, r1: Rho, a2: Sigma, r2: Rho): Tc[Rho] = for {
    _ <- subsCheck(a2, a1)
    rho <- subsCheckRho(r1, r2)
  } yield rho

  def inferSigma(e: Term): Tc[Term] =
    for {
      exp_ty <- inferRho(e)
      env_tys <- getEnvTypes
      env_tvs <- getMetaTyVars(env_tys)
      res_tvs <- getMetaTyVars(Vector(exp_ty))
      res <- quantify(res_tvs.diff(env_tvs), exp_ty).map(e.typed)
    } yield res


  def checkSigma(expr: Term, sigma: Sigma): Tc[Unit] = {
    def bad_tvs(tys: Seq[Type], esc_tvs: Seq[Type]): Seq[Type] =
      tys.filter(esc_tvs.contains)
    for {
      (skol_tvs, rho) <- skolemise(sigma)
      _ <- checkRho(expr, rho)
      env_tys <- getEnvTypes
      esc_tvs <- getFreeTyVars(sigma +: env_tys)
    } yield {
      val bad = bad_tvs(skol_tvs, esc_tvs)
      check(bad.isEmpty, text("Type not polymorphic enough"))
    }
  }

  def instSigma2(t: Term, t1: Sigma, er: Expected[Rho]): Tc[Rho] = {
    val rho = er match {
      case Check(t2) => subsCheckRho(t1, t2)
      case Infer(r) => {
        for {
          t11 <- instantiate(t1)
          r <- writeTcRef(r, t11)
        } yield r
      }
    }
    rho
  }

  def instSigma(t: Term, t1: Sigma, er: Expected[Rho]): Tc[Term] = {
    val rho = er match {
      case Check(t2) => subsCheckRho(t1, t2)
      case Infer(r) => {
        for {
          t11 <- instantiate(t1)
          r <- writeTcRef(r, t11)
        } yield r
      }
    }
    rho.map(t.typed)
  }
}

object TypeCheck extends TypeCheckFunctions