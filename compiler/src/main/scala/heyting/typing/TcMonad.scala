package heyting
package typing

import basictypes.Types._
import basictypes._
import ast._
import prettyprint.Doc
import prettyprint.Docs._
import Tc._

sealed trait Tc[A] {
  def run(env: TcEnv): Either[ErrMsg, A]

  def unTc(env: TcEnv): Either[ErrMsg, A] = run(env)

  def flatMap[B](f: A => Tc[B]): Tc[B] =
    tc(env => {
      unTc(env) match {
        case Left(err) => Left(err)
        case Right(a) => f(a).unTc(env)
      }
    })

  def map[B](f: A => B): Tc[B] = flatMap(a => point(f(a)))

  def extendVarEnv(id: Ident, ty: Sigma): Tc[A] = {
    def extend(env: TcEnv): TcEnv = env.copy(var_env = env.var_env.updated(id, ty))
    tc(env => run(extend(env)))
  }

}

case class TcEnv(uniqs: Ref[Unique], var_env: Map[Ident, Sigma]) {
  def incrUnique: Ref[Unique] = {uniqs.write(uniqs.read.incr); uniqs}
}

object Tc extends TcInstances {

  type ErrMsg = Doc

  def tc[A](f: TcEnv => Either[ErrMsg, A]): Tc[A] = new Tc[A] {
    def run(env: TcEnv) = f(env)
  }

  def point[A](a: A): Tc[A] = tc(_ => Right(a))

  def check(b: Boolean, doc: Doc): Tc[Unit] = if (b) point(()) else failTc(doc)

  def fail[A](err: String): Tc[A] = tc(env => Left(text(err)))

  def failTc[A](err: Doc): Tc[A] = tc(env => Left(err))

  def newUnique: Tc[Unique] = tc[Unique](env => Right(env.incrUnique.read))

  def newTcRef[A](a: A): Tc[Ref[A]] = tc(env => Right(Ref(a)))

  def readTcRef[A](a: Ref[A]): Tc[A] = tc(env => Right(a.read))

  def writeTcRef[A](a: Ref[A], v: A): Tc[Unit] = tc(env => Right(a.write(v)))

  def newTyVarTy: Tc[Tau] = for {
    tv <- newMetaTyVar
  } yield tv

  def newMetaTyVar: Tc[MetaTv] = for {
    u <- newUnique
    tref <- newTcRef[Option[Tau]](None)
  } yield MetaTv(u, tref)

  def newSkolemTyVar(tv: TyVar): Tc[TyVar] = for {
    u <- newUnique
  } yield SkolemTv(tv.name, u)

  def readTv(tv: MetaTv): Tc[Option[Tau]] = readTcRef(tv.tr)

  def writeTv(tv: MetaTv, ty: Tau): Tc[Unit] = writeTcRef(tv.tr, Some(ty))

  def getEnv: Tc[Map[Ident, Sigma]] = tc(env => Right(env.var_env))

  def lookupVar(n: Ident): Tc[Sigma] =
    getEnv.flatMap{env => env.get(n) match {
      case Some(s) => point(s)
      case None => failTc[Sigma](text("Not in scope: ") <+> quotes(Printing.pprName(n)))
    }}

  def instantiate(s: Sigma): Tc[Rho] = s match {
    case ForAll(tvs, ty) => for {
        tvs1 <- tcMonad.mapM(tvs)(_ => newMetaTyVar)
      } yield (Types.substTy(tvs, tvs1.map((aa: MetaTv) => MetaTv(aa.u, aa.tr)), ty))
    case ty => point(ty)
  }

  /**Performs deep skolemisation, returning the skolem constants and the skolemised type.*/
  def skolemise(s: Sigma): Tc[(Seq[TyVar], Rho)] = s match {
    case ForAll(tvs, ty) => for {
      sks1 <- tcMonad.mapM(tvs)(newSkolemTyVar)
      (sks2, ty1) <- skolemise(substTy(tvs, sks1, ty))
    } yield (sks1 ++ sks2, ty1)
    case Fun(arg_ty, res_ty) => for {
      (sks, res_ty1) <- skolemise(res_ty)
    } yield (sks, Fun(arg_ty, res_ty1))
    case ty => point(Vector(), ty)
  }

  //------------------------------------------
  //--      Zonking                         --
  //-- Eliminate any substitutions in the type
  //------------------------------------------
  def zonkType(t: Type): Tc[Type] = t match {
    case ForAll(ns, ty) => zonkType(ty).map(ty1 => ForAll(ns, ty1))
    case Fun(arg, res) => for {
      arg1 <- zonkType(arg)
      res1 <- zonkType(res)
    } yield Fun(arg1, res1)
    case tc: TyCon => point(tc)
    case ty: TyVar => point(ty)
    case tv: MetaTv =>
      readTv(tv).flatMap(mb_ty => mb_ty match {
        case None => point(tv)
        case Some(ty) => for {
          ty1 <- zonkType(ty)
          _ <- writeTv(tv, ty1)
        } yield ty1
      })
  }

  import Printing._
  //------------------------------------------
  //--      Unification                     --
  //------------------------------------------
  def unify(ty1: Tau, ty2: Tau): Tc[Unit] = {
    if (badType(ty1) || badType(ty2)) failTc(text("panic: Unexpected types in unification") <+> vcat(Vector(typeOutput.ppr(ty1), typeOutput.ppr(ty2))))
    else (ty1, ty2) match {
      case (tv1: TyVar, tv2: TyVar) if (tv1 == tv2) =>  point(())
      case (tv1: MetaTv, tv2: MetaTv) if (tv1 == tv2) =>  point(())
      case (tv: MetaTv, ty) => unifyVar(tv, ty)
      case (ty, tv: MetaTv) => unifyVar(tv, ty)
      case (Fun(arg1, res1), Fun(arg2, res2)) => unify(arg1, arg2).flatMap(_ => unify(res1, res2))
      case (tc1: TyCon, tc2: TyCon) if (tc1 == tc2) =>  point(())
      case (ty1, ty2) => failTc(text("Cannot unify types:") <+> vcat(Vector(typeOutput.ppr(ty1), typeOutput.ppr(ty2))))
    }
  }

  def unifyVar(tv1: MetaTv, ty2: Tau): Tc[Unit] =
    readTv(tv1).map(mb_ty1 => mb_ty1 match {
      case Some(ty1) => unify(ty1, ty2)
      case None => unifyUnboundVar(tv1, ty2)
    })


  def unifyUnboundVar(tv1: MetaTv, t: Tau): Tc[Unit] = t match {
    case ty: MetaTv =>
      readTv(ty).map(mb_ty2 => mb_ty2 match {
        case Some(ty1) => unify(tv1, ty1)
        case None => writeTv(tv1, ty)
      })
    case t1 => failTc(text("panic: Unexpected Tau type in unification of unbound var") <+> typeOutput.ppr(t1))
  }


  /**Tells which types should never be encountered during unification.*/
  def badType(ty: Tau): Boolean = ty match {
    case BoundTv(_) => true
    case _ => false
  }
}

trait TcInstances {
  implicit def tcMonad = new Monad[Tc] {
    def flatMap[A, B](fa: Tc[A])(f: (A) => Tc[B]) = fa.flatMap(f)

    def point[A](a: => A) = Tc.point(a)
  }
}