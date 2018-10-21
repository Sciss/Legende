/*
 *  Util.scala
 *  (Légende)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.legende

import de.sciss.file._

object Util {
  def formatTemplate(f: File, args: Any*): File = {
    val name = f.name.format(args: _*)
    f.replaceName(name)
  }
}
