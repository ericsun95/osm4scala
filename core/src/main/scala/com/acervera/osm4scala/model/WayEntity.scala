/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Ángel Cervera Claudio
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package com.acervera.osm4scala.model

import org.openstreetmap.osmosis.osmbinary.osmformat.{Info, StringTable, Way}

/**
  * Entity that represent a OSM way as https://wiki.openstreetmap.org/wiki/Elements#Way and https://wiki.openstreetmap.org/wiki/Way describe
  */
case class WayEntity(id: Long,
                     nodes: Seq[Long],
                     tags: Map[String, String],
                     version: Option[Int],
                     timestamp: Option[Long],
                     changeset: Option[Long],
                     uid: Option[Int],
                     user_sid: Option[Int],
                     visible: Option[Boolean]) extends OSMEntity {

  override val osmModel: OSMTypes.Value = OSMTypes.Way
  override def toString: String = {
    s"Way id: ${id}, " +
      s"nodes: ${nodes}, " +
      s"tags: ${tags.toList}, " +
      s"version: ${version.getOrElse("None")}," +
      s"timestamp: ${timestamp.getOrElse("None")}, " +
      s"changeset: ${changeset.getOrElse("None")}, " +
      s"uid: ${uid.getOrElse("None")}, " +
      s"user_sid: ${user_sid.getOrElse("None")}, " +
      s"visible: ${visible.getOrElse("True")}"
  }

  object WayEntityTypes extends Enumeration { // TODO: How to know the type ?????
    val Open, Close, Area, CombinedClosedPolylineArea = Value
  }

}

object WayEntity {

  def apply(osmosisStringTable: StringTable, osmosisWay: Way): WayEntity = {

    // Calculate nodes references in stored in delta compression.
    val nodes: Seq[Long] = osmosisWay.refs.scanLeft(0L) { _ + _ }.drop(1)
    val optionalInfo: Option[Info] = osmosisWay.info

    // Calculate tags using the StringTable.
    val tags = (osmosisWay.keys, osmosisWay.vals).zipped.map { (k, v) =>
      osmosisStringTable.s(k).toString("UTF-8") -> osmosisStringTable.s(v).toString("UTF-8")
    }.toMap

    WayEntity(
      id = osmosisWay.id,
      nodes = nodes,
      tags = tags,
      version = if(optionalInfo.isDefined) optionalInfo.get.version else None,
      timestamp = if(optionalInfo.isDefined) optionalInfo.get.timestamp else None,
      changeset = if(optionalInfo.isDefined) optionalInfo.get.changeset else None,
      uid = if(optionalInfo.isDefined) optionalInfo.get.uid else None,
      user_sid = if(optionalInfo.isDefined) optionalInfo.get.userSid else None,
      visible = if(optionalInfo.isDefined) optionalInfo.get.visible else None
    )
  }

}
