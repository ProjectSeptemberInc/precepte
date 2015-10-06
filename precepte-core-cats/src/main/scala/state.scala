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

import shapeless._
import shapeless.ops.hlist.{ Selector, ToCoproduct }
import shapeless.ops.coproduct.{ Basis }

import scala.language.implicitConversions
import scala.language.higherKinds

trait SubList[Super <: HList, S <: HList] extends DepFn1[Super] with Serializable {
  type Out = S
}

object SubList {

  implicit def hnilSubList[Super <: HList] = new SubList[Super, HNil] {
    def apply(s: Super) = HNil
  }

  implicit def normalSubList[H, Super <: HList, S <: HList](
    implicit sel: Selector[Super, H], sub: SubList[Super, S]
  ) = new SubList[Super, H :: S] {
    def apply(s: Super): H :: sub.Out = sel(s) :: sub(s)
  }
}

package object state {

  implicit class UnifyStateUnit[Ta, MS, F[_], A](val p: Precepte[Ta, MS, Unit, F, A]) extends AnyVal {
    def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2]): Precepte[Ta, MS, UMS2, F, A] = {      
      p.xmapState[UMS2]((_:Unit) => pivot.um, (_:UMS2) => ())
    }

  }


  implicit class UnifyState[Ta, MS, UMS, F[_], A](val p: Precepte[Ta, MS, UMS, F, A]) extends AnyVal {
    // def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2])(implicit gen: Generic.Aux[UMS2, R], sel: Selector[R, UMS]): Precepte[Ta, MS, UMS2, F, A] = {
    //   p.xmapState[UMS2]((_:UMS) => pivot.um, (_:UMS2) => sel(gen.to(pivot.um)))
    // }

    def unify[UMS2, HL <: HList, C <: Coproduct, HL2 <: HList, C2 <: Coproduct](pivot: PState[Ta, MS, UMS2])(
      implicit
        gen: Generic.Aux[UMS, HL],
        gen2: Generic.Aux[UMS2, HL2],
        subList: SubList[HL2, HL]
    ): Precepte[Ta, MS, UMS2, F, A] = {
      p.xmapState[UMS2]((_:UMS) => pivot.um, (_:UMS2) => gen.from(subList(gen2.to(pivot.um))))
    }

  }

}