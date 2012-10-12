package heyting
package basictypes



trait Monad[F[_]] {
  import ToMonadOps._
  def point[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => point(f(a)))

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = flatMap(fa)(f)

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def >>=[A, B](fa: F[A])(f: A => F[B]): F[B] = flatMap(fa)(f)

  def flatMap_[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fa)(_ => fb)

  def mapM[A, B](ms: Seq[A])(f: A => F[B])(implicit F: Monad[F]): F[Seq[B]] =
    sequence(ms.map(f))

  def mapM_[A, B](ms: Seq[A])(f: A => F[B])(implicit F: Monad[F]): F[Unit] =
    sequence_(ms.map(f))

  def sequence[A](ms: Seq[F[A]])(implicit F: Monad[F]): F[Seq[A]] =
    ms.foldLeft(point[Seq[A]](Vector()))((fsa: F[Seq[A]], fa: F[A]) =>
      F.flatMap(fa)(x => F.map(fsa)((xsa: Seq[A]) => x +: xsa))
    )

  def sequence_[A](ms: Seq[F[A]])(implicit F: Monad[F]): F[Unit] = {
    ms.foldLeft(point[Unit](()))((fsa, fa) => F.flatMap_(fa)(fsa))
  }
}

trait MonadOps[F[_],A] {
  def self: F[A]
  implicit def F: Monad[F]

  def >>=[B](f: A => F[B]): F[B] = F.flatMap(self)(f)

  def >>[B](fb: F[B]): F[B] = F.flatMap(self)(_ => fb)
}

object ToMonadOps {
  implicit def ToMonadOps[F[_],A](v: F[A])(implicit F0: Monad[F]) =
    new MonadOps[F,A] { def self = v; implicit def F: Monad[F] = F0 }

}