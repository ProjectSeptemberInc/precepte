/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte
package corecats

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

class SI2712Spec extends FlatSpec with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import cats.std.future._
  import cats.Applicative
  import cats.Functor
  import cats.syntax.flatMap._
  import cats.syntax.apply._
  import cats.syntax.functor._
  import cats.data.XorT

  import default._
  import Macros._

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  private def tags(n: String) = BaseTags(Callee(n), Category.Database)

  def nostate = ST(Span.gen, env, Vector.empty, ())

  implicit val unitSG = new cats.Semigroup[Unit] {
    def combine(f1: Unit, f2: Unit) = ()
  }

  "Precepte" should "unify in function" in {
    import cats.data.OptionT

    def f[F[_] : Functor, A](fa: F[A]): F[Option[A]] = fa.map(Some(_))

    val p: DefaultPre[Future, Unit, Int] = Pre.pure(5)
    /*
      Without -Yhigher-order-unification, it fails at compile-time

      [error]  found   : com.mfglabs.precepte.default.DefaultPre[scala.concurrent.Future,Unit,Int]
      [error]     (which expands to)  com.mfglabs.precepte.Precepte[com.mfglabs.precepte.default.BaseTags,com.mfglabs.precepte.default.ManagedState[com.mfglabs.precepte.default.BaseEnv,com.mfglabs.precepte.default.BaseTags],Unit,scala.concurrent.Future,Int]
      [error]  required: ?F[?A]
      [error] Note that implicit conversions are not applicable because they are ambiguous:
      [error]  both method ArrowAssoc in object Predef of type [A](self: A)ArrowAssoc[A]
      [error]  and method Ensuring in object Predef of type [A](self: A)Ensuring[A]
      [error]  are possible conversion functions from com.mfglabs.precepte.default.DefaultPre[scala.concurrent.Future,Unit,Int] to ?F[?A]
      [error]     f(p).eval(nostate).futureValue should equal (Some(5))      
    */
    f(p).eval(nostate).futureValue should equal (Some(5))

  }

  it should "unify in monad trans" in {
    import cats.data.OptionT

    val p = Applicative[DefaultPre[Future, Unit, ?]].pure(Option(1))
    /*
      Without -Yhigher-order-unification, it fails at compile-time

      [error] precepte/precepte-core-cats/src/test/scala/SI2712Spec.scala:99: no type parameters for method apply: (value: F[Option[A]])cats.data
      [error]  --- because ---
      [error] argument expression's type is not compatible with formal parameter type;
      [error]  found   : com.mfglabs.precepte.Precepte[com.mfglabs.precepte.default.BaseTags,com.mfglabs.precepte.default.ManagedState[com.mfglabs.precepte.default.BaseEnv,com.mfglabs.precepte.default.BaseTags],Unit,scala.concurrent.Future,Option[Int]]
      [error]  required: ?F[Option[?A]]
      [error]     OptionT(f1).filter(_ => true).filter(_ => true).value.eval(nostate).futureValue should ===(Some(1))
      [error]     ^
      [error] precepte/precepte-core-cats/src/test/scala/SI2712Spec.scala:79: type mismatch;
      [error]  found   : com.mfglabs.precepte.Precepte[com.mfglabs.precepte.default.BaseTags,com.mfglabs.precepte.default.ManagedState[com.mfglabs.precepte.default.BaseEnv,com.mfglabs.precepte.default.BaseTags],Unit,scala.concurrent.Future,Option[Int]]
      [error]  required: F[Option[A]]
      [error]     OptionT(f1).filter(_ => true).filter(_ => true).value.eval(nostate).futureValue should ===(Some(1))
    */
    OptionT(p).filter(_ => true).filter(_ => true).value.eval(nostate).futureValue should ===(Some(1))

  }
}