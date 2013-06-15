/*
  This file is part of Octetoscope.
  Copyright (C) 2013 Octetoscope contributors (see /AUTHORS.txt)

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

import org.gradle.api.Project

import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.lib.RepositoryBuilder
import org.eclipse.jgit.revwalk.RevWalk

public class BuildUtils {
  public static List getVersionInfo(Project project, String tagPrefix) {
    def repo = new RepositoryBuilder().setWorkTree(project.rootDir).build()

    def walk = new RevWalk(repo)

    def tagMap = repo.tags.findAll { it.key.startsWith(tagPrefix) }.collectEntries {
      [walk.peel(walk.parseTag(it.value.objectId)).copy(), it.key]
    }

    def curCommit = walk.lookupCommit(repo.resolve(Constants.HEAD))
    def distanceToTag = 0

    while (!tagMap.containsKey(curCommit.id)) {
      walk.parseHeaders(curCommit)
      curCommit = curCommit.parents[0]
      ++distanceToTag
    }

    return [repo.resolve(Constants.HEAD).name(), distanceToTag, tagMap[curCommit.id].toString()]
  }
}
