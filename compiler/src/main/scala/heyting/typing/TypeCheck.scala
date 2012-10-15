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

  def tcExpr(e: Term): Tc[Sigma] =
    inferSigma(e).flatMap(ty => zonkType(ty))

  final def checkRho(expr: Term, ty: Rho): Tc[Unit] = tcRho(expr, Check(ty))

  /**
   * Type check a Rho term.
   * Invariant: if the second argument is (Check rho),
   * then rho is in weak-prenex form
   */
  def tcRho(t: Term, e: Expected[Rho]): Tc[Unit] = {
    (t, e) match {
      case (Lit(l), exp_ty) => instSigma(litType(l), exp_ty)
      case (Var(v), exp_ty) => for {
        v_sigma <- lookupVar(v)
      } yield instSigma(v_sigma, exp_ty)
      case (App(fun, arg), exp_ty) => for {
        fun_ty <- inferRho(fun)
        (arg_ty, res_ty) <- unifyFun(fun_ty)
        _ <- checkSigma(arg, arg_ty)
      } yield instSigma(res_ty, exp_ty)
      case (Lam(v, body), Check(exp_ty)) => for {
        (var_ty, body_ty) <- unifyFun(exp_ty)
      } yield (checkRho(body, body_ty)).extendVarEnv(v, var_ty)
      case (Lam(v, body), Infer(ref)) => for {
        var_ty <- newTyVarTy
        body_ty <- inferRho(body).extendVarEnv(v, var_ty)
      } yield writeTcRef(ref, var_ty --> body_ty)
      case (ALam(v, var_ty, body), Check(exp_ty)) => for {
        (arg_ty, body_ty) <- unifyFun(exp_ty)
        _ <- subsCheck(arg_ty, var_ty)
      } yield checkRho(body, body_ty).extendVarEnv(v, var_ty)
      case (ALam(v, var_ty, body), Infer(ref)) => for {
        body_ty <- inferRho(body).extendVarEnv(v, var_ty)
      } yield writeTcRef(ref, var_ty --> body_ty)
      case (Let(v, rhs), Infer(ref)) => for {
        var_ty <- newTyVarTy
        body_ty <- inferRho(rhs).extendVarEnv(v, var_ty)
      } yield writeTcRef(ref, body_ty)
      case (Let(v, rhs), exp_ty) => for {
        var_ty <- newTyVarTy
      } yield tcRho(rhs, exp_ty).extendVarEnv(v, var_ty)
      case (Ann(body, ann_ty), exp_ty) => for {
        _ <- checkSigma(body, ann_ty)
      } yield instSigma(ann_ty, exp_ty)
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
  def subsCheckRho(s: Sigma, r: Rho): Tc[Unit] = (s, r) match {
    case (sigma1@ForAll(_, _), rho2) => for {
      rho1 <- instantiate(sigma1)
    } yield subsCheckRho(rho1, rho2)
    case (rho1, Fun(a2, r2)) => for {
      (a1, r1) <- unifyFun(rho1)
    } yield subsCheckFun(a1, r1, a2, r2)
    case (Fun(a1, r1), rho2) => for {
      (a2, r2) <- unifyFun(rho2)
    } yield subsCheckFun(a1, r1, a2, r2)
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

  def subsCheckFun(a1: Sigma, r1: Rho, a2: Sigma, r2: Rho): Tc[Unit] = for {
    _ <- subsCheck(a2, a1)
  } yield subsCheckRho(r1, r2)

  def inferSigma(e: Term): Tc[Sigma] =
    for {
      exp_ty <- inferRho(e)
      env_tys <- getEnvTypes
      env_tvs <- getMetaTyVars(env_tys)
      res_tvs <- getMetaTyVars(Vector(exp_ty))
      res <- quantify(res_tvs.diff(env_tvs), exp_ty)
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

  def instSigma(t1: Sigma, er: Expected[Rho]): Tc[Unit] = {
    er match {
      case Check(t2) => subsCheckRho(t1, t2)
      case Infer(r) => {
        for {
          t11 <- instantiate(t1)
        } yield writeTcRef(r, t11)
      }
    }
  }
}

object TypeCheck extends TypeCheckFunctions