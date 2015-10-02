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

import shapeless.{ Generic, HList, HNil, :: }
import shapeless.ops.hlist.Selector

import scala.language.implicitConversions
import scala.language.higherKinds


package object state {

  implicit class UnifyStateUnit[Ta, MS, F[_], A](val p: Precepte[Ta, MS, Unit, F, A]) extends AnyVal {
    def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2]): Precepte[Ta, MS, UMS2, F, A] = {      
      p.xmapState[UMS2]((_:Unit) => pivot.um, (_:UMS2) => ())
    }

  }


  implicit class UnifyState[Ta, MS, UMS, F[_], A](val p: Precepte[Ta, MS, UMS, F, A]) extends AnyVal {
    def unify[UMS2, R <: HList](pivot: PState[Ta, MS, UMS2])(implicit gen: Generic.Aux[UMS2, R], sel: Selector[R, UMS]): Precepte[Ta, MS, UMS2, F, A] = {
      p.xmapState[UMS2]((_:UMS) => pivot.um, (_:UMS2) => sel(gen.to(pivot.um)))
    }

  }

  // implicit def UnifyStateUnitImpl[Ta, MS, UMS2, F[_], A](p: Precepte[Ta, MS, Unit, F, A])(
  //   implicit pivot: PState[Ta, MS, UMS2]
  // ): Precepte[Ta, MS, UMS2, F, A] = {
  //   (new UnifyStateUnit(p)).unify(pivot)
  // }

  // implicit def UnifyStateImpl[Ta, MS, UMS, UMS2, F[_], A, R <: HList](p: Precepte[Ta, MS, UMS, F, A])(
  //   implicit pivot: PState[Ta, MS, UMS2], gen: Generic.Aux[UMS2, R], sel: Selector[R, UMS]
  // ) = {
  //   (new UnifyState(p)).unify(pivot)
  // }

}