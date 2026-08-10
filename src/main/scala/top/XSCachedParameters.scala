// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package top

import org.chipsalliance.cde.config.{View, Field, Parameters}

import scala.collection.mutable


private[top] object XSCachedParameters {
  def cached(underlying: Parameters): Parameters = new CachedParameters(underlying)
  def apply(underlying: Parameters): Parameters = cached(underlying)

  private class CachedParameters(underlying: Parameters) extends Parameters {
    private val cache = mutable.HashMap.empty[Field[_], Option[Any]]

    override def find[T](key: Field[T]): Option[T] =
      cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[Option[T]]

        case None =>
          val value = underlying.lift(key)
          cache(key) = value
          value
      }

    override def chain[T](
      site:  View,
      here:  View,
      up:    View,
      pname: Field[T]
    ): Option[T] = { 
      throw new Exception("The XSCachedParameters is frozen thus cannot participate in alter/orElse")
    }
  }
}

private[top] object XSCachedParametersOptional {
  def apply(enable:Boolean, p: Parameters) : Parameters = if (enable) XSCachedParameters(p) else p
  def apply(enable: Option[Boolean], p: Parameters) : Parameters  = enable match {
    case Some(b) => apply(b,p)
    case None => p
  } 
}

private[top] case object CachedParameterKey extends Field[Boolean](false)