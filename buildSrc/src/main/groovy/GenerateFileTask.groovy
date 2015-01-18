/*
  This file is part of Octetoscope.
  Copyright (C) 2015 Octetoscope contributors (see /AUTHORS.txt)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.OutputFile
import org.gradle.api.tasks.TaskAction

class GenerateFileTask extends DefaultTask {
  String text = ""

  @OutputFile
  File outputPath

  GenerateFileTask() {
    outputs.upToDateWhen {
      try { outputPath.getText("US-ASCII") == text }
      catch (IOException ignored) { false }
    }
  }

  @TaskAction
  def generate() {
    outputPath.parentFile.mkdirs()
    outputPath.write(text)
  }
}
