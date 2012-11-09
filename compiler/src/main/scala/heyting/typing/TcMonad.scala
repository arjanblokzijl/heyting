package heyting
package typing

import basictypes._
import Printing._
import basictypes.Types._
import ast._
import prettyprint.Doc
import prettyprint.Docs._
import Tc._

sealed trait Tc[A] {
  def run(env: TcEnv): Either[ErrMsg, A]

  def unTc(env: TcEnv): Either[ErrMsg, A] = run(env)

  def flatMap[B](f: A => Tc[B]): Tc[B] =
    tc(env =>
      run(env) match {
        case Left(err) => Left(err)
        case Right(a) => f(a).run(env)
    })

  def map[B](f: A => B): Tc[B] =
    flatMap(a => point(f(a)))

  def >>=[B](f: A => Tc[B]): Tc[B] = flatMap(f)

  def >>[B](fb: Tc[B]): Tc[B] = flatMap(_ => fb)

  def extendVarEnv(id: Ident, ty: Sigma): Tc[A] = {
    println("extendVarEnv for ident %s and sigma %s" format(id, ty))
    def extend(env: TcEnv): TcEnv = env.copy(var_env = env.var_env.updated(id, ty))
    tc(env => {
      val eenv = extend(env)
      println("extendVarEnv before extend: " + env)
      println("extendVarEnv after extend: " + extend(eenv))
      run(eenv)
    })
  }

}

case class TcEnv(uniqs: Ref[Unique], var_env: Map[Ident, Sigma]) {
  def incrUnique: Ref[Unique] = {
    val incr: Unique = uniqs.read.incr
    uniqs.write(incr)
    uniqs
  }
}

object Tc extends TcInstances with TcFunctions {

  type ErrMsg = Doc

}

trait TcFunctions {
  def tc[A](f: TcEnv => Either[ErrMsg, A]): Tc[A] = new Tc[A] {
    def run(env: TcEnv) = f(env)
  }

  def point[A](a: A): Tc[A] =
    tc(_ => Right(a))

  def check(b: Boolean, doc: Doc): Tc[Unit] = if (b) point(()) else failTc(doc)

  def fail[A](err: String): Tc[A] = tc(env => Left(text(err)))

  def failTc[A](err: Doc): Tc[A] = tc(env => Left(err))

  def runTc[A](binds: Seq[(Ident, Sigma)], tc: Tc[A]): Either[ErrMsg, A] = {
    val env = TcEnv(Ref(Unique.newUnique), var_env = binds.toMap)
    tc.run(env)
  }

  def newUnique: Tc[Unique] = tc[Unique](env => {
    val newU = env.incrUnique
    val incr = newU.read
    Right(incr)
  })

  def newTcRef[A](a: => A): Tc[Ref[A]] = tc(env => Right(Ref(a)))

  def readTcRef[A](a: Ref[A]): Tc[A] = tc(env => Right(a.read))

  def writeTcRef[A](a: Ref[A], v: A): Tc[A] = {
    val a1 = a.write(v) //need to evaluate this first
    tc[A](env => Right[ErrMsg, A](a1))
  }

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

  def writeTv(tv: MetaTv, ty: Tau): Tc[Tau] = writeTcRef(tv.tr, Some(ty)).map(_.get)

  def getEnv: Tc[Map[Ident, Sigma]] = tc(env => Right(env.var_env))

  def getEnvTypes: Tc[Seq[Type]] = for {
    env <- getEnv
  } yield env.values.toIndexedSeq

  def extendVarEnv[A](v: Ident, ty: Sigma, m: Tc[A]): Tc[A] =
    m.extendVarEnv(v, ty)

  def lookupVar(n: Ident): Tc[Sigma] = {
    getEnv.flatMap{env => {
      env.get(n) match {
        case Some(s) => point(s)
        case None => failTc[Sigma](text("Not in scope: ") <+> quotes(Printing.pprName(n)))
      }
    }}
  }

  def instantiate(s: Sigma): Tc[Rho] = s match {
    case ForAll(tvs, ty) => for {
      tvs1 <- tcMonad.mapM(tvs)(_ => newMetaTyVar)
    } yield (Types.substTy(tvs, tvs1.map((aa: MetaTv) => MetaTv(aa.u, aa.tr)), ty))
    case ty => point(ty)
  }


//  ------------------------------------------
//  --      Quantification                  --
//  ------------------------------------------
  def quantify(tvs: Seq[MetaTv], ty: Rho): Tc[Sigma] = {
    val used_bndrs = tyVarBndrs(ty)
    val new_bndrs: Seq[TyVar] = allBinders.diff(used_bndrs).take(tvs.length)
    def bind(tv: MetaTv, name: String): Tc[Sigma] = writeTv(tv, BoundTv(name))
    for {
      _ <- tcMonad.mapM_[(MetaTv, TyVar), Sigma](tvs.zip(new_bndrs)){case (tv, ty1) => bind(tv, ty1.name)}
      ty2 <- zonkType(ty)
    } yield ForAll(new_bndrs, ty2)
  }

  //TODO this is not great, perhaps introduce some function allocating new types based on the used ones.
  private final val allChars: Seq[Char] = ('a' to 'z').toIndexedSeq
  private final val allInts: Seq[Int] = (1 to 100).toIndexedSeq

  private final val allBinders: Seq[TyVar] = {
    val names = allChars.map(c => BoundTv(c.toString))
    val idxNames = for {
      i <- allInts
      x <- allChars
    } yield BoundTv(x.toString + i.toString)
    names ++ idxNames
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


  /** This function takes account of zonking, and returns a set
    * (no duplicates) of free type variables
    */
  def getFreeTyVars(tys: Seq[Type]): Tc[Seq[Type]] =
    for {
      tys1 <- tcMonad.mapM(tys)(zonkType)
    } yield freeTyVars(tys1)

  /**
    * This function takes account of zonking, and returns a set
    * (no duplicates) of unbound meta-type variables.
    */
  def getMetaTyVars(tys: Seq[Type]): Tc[Seq[MetaTv]] =
    for {
      tys1 <- tcMonad.mapM(tys)(zonkType)
    } yield metaTvs(tys1)


  /**
   * The instantiation of type variables, aka as Zonking in GHC.
   * Eliminates any substitutions in the type
  */
  def zonkType(t: Type): Tc[Type] = {
    t match {
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
  }


  //------------------------------------------
  //--      Unification                     --
  //------------------------------------------
  def unify(ty1: Tau, ty2: Tau): Tc[Tau] = {
    if (badType(ty1) || badType(ty2)) failTc(text("panic: Unexpected types in unification") <+> vcat(Vector(typeOutput.ppr(ty1), typeOutput.ppr(ty2))))
    else (ty1, ty2) match {
      case (tv1: TyVar, tv2: TyVar) if (tv1 == tv2) =>  point(tv1)
      case (tv1: MetaTv, tv2: MetaTv) if (tv1 == tv2) =>  point(tv1)
      case (tv: MetaTv, ty) => unifyVar(tv, ty)
      case (ty, tv: MetaTv) => unifyVar(tv, ty)
      case (Fun(arg1, res1), Fun(arg2, res2)) => unify(arg1, arg2).flatMap(_ => unify(res1, res2))
      case (tc1: TyCon, tc2: TyCon) if (tc1 == tc2) =>  point(tc1)
      case (ty1, ty2) => failTc(text("Cannot unify types:") <+> vcat(Vector(typeOutput.ppr(ty1), typeOutput.ppr(ty2))))
    }
  }

  def unifyVar(tv1: MetaTv, ty2: Tau): Tc[Tau] =
    readTv(tv1).flatMap(mb_ty1 => mb_ty1 match {
      case Some(ty1) => unify(ty1, ty2)
      case None => unifyUnboundVar(tv1, ty2)
    })


  def unifyUnboundVar(tv1: MetaTv, ty1: Tau): Tc[Tau] = ty1 match {
    case ty: MetaTv =>
      readTv(ty).flatMap(mb_ty2 => mb_ty2 match {
        case Some(ty1) => unify(tv1, ty1)
        case None => writeTv(tv1, ty)
      })
    case ty2 =>
      getMetaTyVars(IndexedSeq(ty2)).flatMap(tvs2 =>
        if (tvs2.contains(tv1)) occursCheckErr[Tau](tv1, ty2)
        else writeTv(tv1, ty2))
  }

  def unifyFun(r: Rho): Tc[(Sigma, Rho)] = r match {
    case Fun(arg, res) => point(arg, res)
    case tau => for {
      arg_ty <- newTyVarTy
      res_ty <- newTyVarTy
      _ <- unify(tau, arg_ty --> res_ty)
    } yield (arg_ty, res_ty)
  }

  /**Tells which types should never be encountered during unification.*/
  def badType(ty: Tau): Boolean = ty match {
    case BoundTv(_) => true
    case _ => false
  }

  def occursCheckErr[A](tv: MetaTv, ty: Tau): Tc[A] =
    failTc[A](text("Occurs check for") <+> quotes(typeOutput.ppr(tv)) <+> text("in") <+> typeOutput.ppr(ty))
}

trait TcInstances {
  implicit def tcMonad = new Monad[Tc] {
    def flatMap[A, B](fa: Tc[A])(f: A => Tc[B]) = fa.flatMap(f)

    def point[A](a: => A) = Tc.point(a)
  }
}