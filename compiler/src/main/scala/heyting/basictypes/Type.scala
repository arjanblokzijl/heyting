package heyting
package basictypes

import scala.annotation._
import Types._

sealed trait Type {
  def -->(res: Sigma): Tau = Fun(this, res)
}
case class ForAll(tyVars: Seq[TyVar], rho: Rho) extends Type
case class Fun(arg: Type, res: Type) extends Type
sealed trait TyCon extends Type

sealed trait TyVar extends Type {
  def name: String = this match {
    case BoundTv(n) => n
    case SkolemTv(n, u) => n
  }
}


object Types {

  /**a type variable bound by a ForAll*/
  case class BoundTv(s: String) extends TyVar
  /**A skolem constraint. The string is just to improve the error message.*/
  case class SkolemTv(s: String, u: Uniq) extends TyVar {
    override def equals(other: Any) = other match {
      case SkolemTv(_, u2) => u == u2
      case _ => false
    }
    override def hashCode() = u.hashCode
  }

  case class MetaTv(u: Unique, tr: TyRef) extends Type {
    override def equals(other: Any) = other match {
      case MetaTv(u2, _) => u == u2
      case _ => false
    }

    override def hashCode() = u.hashCode
  }

  type Uniq = Unique
  type Rho = Type
  type Sigma = Type//-- No top-level ForAll
  type Tau = Type//-- No ForAlls anywhere
  type TyRef = Ref[Option[Tau]]

  //type constants
  case object IntT extends TyCon
  case object DoubleT extends TyCon
  case object BooleanT extends TyCon
  case object StringT extends TyCon
  case object CharT extends TyCon
  case object FloatT extends TyCon

  type Env = Seq[(TyVar, Tau)]

  def lookup(ty: TyVar, env: Env): Option[Tau] = env.find(tt => tt._1 == ty).map(_._2)

  //Free and bound variables

  /**gets the MetaTvs from a tyoe: no duplicates in the result.*/
  def metaTvs(tys: Seq[Type]): Seq[MetaTv] = {
    def go(tv: Type, acc: IndexedSeq[MetaTv]): IndexedSeq[MetaTv] = tv match {
      case mt: MetaTv => {
        if (acc.contains(tv)) acc
        else mt +: acc
      }
      case t: TyVar => acc
      case t: TyCon => acc
      case Fun(arg, res) => go(arg, go(res, acc))
      case ForAll(_, ty) => go(ty, acc)
    }
    tys.foldLeft(IndexedSeq[MetaTv]())((acc, tpe) => go(tpe, acc))
  }

  /**Gets the free type variables from a Type. No duplicates in the result.*/
  def freeTyVars(tys: Seq[Type]): Seq[TyVar] = {
    def go(bound: Seq[TyVar], tpe: Type, acc: IndexedSeq[TyVar]): IndexedSeq[TyVar] = tpe match {
      case tv: TyVar => {
        if (bound.contains(tv)) acc
        else if (acc.contains(tv)) acc
        else tv +: acc
      }
      case mtv: MetaTv => acc
      case tyCon: TyCon => acc
      case Fun(arg, res) => go(bound, arg, go(bound, res, acc))
      case ForAll(tvs, ty) => go(tvs ++ bound, ty, acc)
    }
    tys.foldLeft(IndexedSeq[TyVar]())((acc, tpe) => go(IndexedSeq(), tpe, acc))
  }

  /**
   * Get all the binders used in ForAlls in the type, so that
   * when quantifying an outer for-all we can avoid these inner ones
   */
  def tyVarBndrs(ty: Rho): Seq[TyVar] = {
    def bndrs(tpe: Type): Seq[TyVar] = tpe match {
      case ForAll(tvs, body) => tvs ++ bndrs(body)
      case Fun(arg, res) => bndrs(arg) ++ bndrs(res)
      case _ => IndexedSeq()
    }
    bndrs(ty).distinct
  }

  //substitution
  def substTy(tvs: Seq[TyVar], tys: Seq[Type], ty: Type): Type = {
    def subst_ty(env: Env, tpe: Type): Type = tpe match {
      case Fun(arg, res) => Fun(subst_ty(env, arg), subst_ty(env, res))
      case t: TyVar => lookup(t, env).getOrElse(t)
      case tv: MetaTv => tv
      case tc: TyCon => tc
      case ForAll(ns, rho) => {
        def env1 = env.filter(tt => ns.contains(tt._1))
        ForAll(ns, subst_ty(env1, rho))
      }
    }
    subst_ty(tvs.zip(tys), ty)
  }
}