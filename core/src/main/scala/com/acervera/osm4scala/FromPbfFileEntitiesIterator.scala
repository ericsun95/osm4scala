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

package com.acervera.osm4scala

import com.acervera.osm4scala.model.OSMEntity

/**
  * Iterator over all entities in a pbf file, or until the Sentinel stop it.
  * Every item is a OSMEntity object.
  *
  * @param pbfInputStream Input stream with Sentinel logic.
  * @author angelcervera
  */
class FromPbfFileEntitiesIterator(pbfInputStream: InputStreamSentinel) extends EntityIterator {

  // Iterator over OSMData blocks
  private val blobIterator = BlobTupleIterator
    .fromPbf(pbfInputStream)
    .withFilter { case (blobHeader, _) => blobHeader.`type` == "OSMData" }

  // Iterator entities in active block
  private var osmEntitiesIterator: Option[EntityIterator] = readNextBlock()

  override def hasNext: Boolean =
    osmEntitiesIterator.isDefined && (osmEntitiesIterator.get.hasNext || blobIterator.hasNext)

  override def next(): OSMEntity = {
    val nextEntity = osmEntitiesIterator.get.next

    if (!osmEntitiesIterator.get.hasNext) {
      osmEntitiesIterator = readNextBlock()
    }

    nextEntity
  }

  /**
    * Read the next osm pbf block
    */
  private def readNextBlock(): Option[EntityIterator] =
    if (blobIterator.hasNext) {
      Some(EntityIterator.fromBlob(blobIterator.next._2))
    } else {
      None
    }

}
