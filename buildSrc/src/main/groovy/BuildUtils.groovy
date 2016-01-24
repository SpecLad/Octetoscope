/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2016 Octetoscope contributors (see /AUTHORS.txt)

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

import org.eclipse.jgit.dircache.DirCacheIterator
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.lib.FileMode
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.lib.RepositoryBuilder
import org.eclipse.jgit.revwalk.RevTree
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.AbstractTreeIterator
import org.eclipse.jgit.treewalk.FileTreeIterator
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.NotIgnoredFilter
import org.gradle.api.Project

public final class BuildUtils {
  private BuildUtils() {}

  private static boolean checkDirty(Repository repo, RevTree baseTree) {
    def treeWalk = new TreeWalk(repo)

    treeWalk.addTree(baseTree)
    treeWalk.addTree(new FileTreeIterator(repo))

    treeWalk.recursive = true
    treeWalk.filter = new NotIgnoredFilter(1)

    while (treeWalk.next()) {
      def baseItem = treeWalk.getTree(0, AbstractTreeIterator.class)
      def wtItem = treeWalk.getTree(1, AbstractTreeIterator.class)

      if (baseItem != null && wtItem != null) {
        if (baseItem.entryObjectId != wtItem.entryObjectId)
          return true
      } else if (baseItem != wtItem) {
        return true
      }
    }

    return false
  }

  public static VersionInfo getVersionInfo(Project project, String tagPrefix) {
    def repo = new RepositoryBuilder().setWorkTree(project.rootDir).build()

    try {
      def tagMap = repo.tags.findAll { it.key.startsWith(tagPrefix) }.collectEntries {
        [repo.peel(it.value).peeledObjectId, it.key]
      }

      def walk = new RevWalk(repo)

      try {
        def headId = repo.resolve(Constants.HEAD)

        def curCommit = walk.lookupCommit(headId)
        def distanceToTag = 0

        while (!tagMap.containsKey(curCommit.id)) {
          walk.parseHeaders(curCommit)
          curCommit = curCommit.parents[0]
          ++distanceToTag
        }

        return new VersionInfo(
            releaseVersion: tagMap[curCommit.id].toString().substring(tagPrefix.length()),
            extraCommits: distanceToTag,
            commitHash: headId.name(),
            dirty: checkDirty(repo, walk.parseCommit(headId).tree)
        )
      } finally {
        walk.dispose()
      }
    } finally {
      repo.close()
    }
  }

  public static List<ExportedFileInfo> getExportedFiles(Project project) {
    def repo = new RepositoryBuilder().setWorkTree(project.rootDir).build()

    try {
      def treeWalk = new TreeWalk(repo)

      treeWalk.addTree(new FileTreeIterator(repo))
      treeWalk.addTree(new DirCacheIterator(repo.readDirCache()))

      treeWalk.recursive = true
      treeWalk.filter = new NotIgnoredFilter(0)

      def results = []

      while (treeWalk.next()) {
        def wtItem = treeWalk.getTree(0, FileTreeIterator.class)
        def dcItem = treeWalk.getTree(1, DirCacheIterator.class)

        if (wtItem == null) continue

        def mode = wtItem.getIndexFileMode(dcItem)

        if (mode != FileMode.REGULAR_FILE && mode != FileMode.EXECUTABLE_FILE)
          throw new RuntimeException("Can't export an entry with mode $mode.")

        def attrs = treeWalk.getAttributes()

        results.add(new ExportedFileInfo(
            path: wtItem.entryPathString,
            executable: mode == FileMode.EXECUTABLE_FILE,
            containsNativeText: !attrs.isCustom("eol") && attrs.isSet("text")))
      }

      return results
    } finally {
      repo.close()
    }
  }
}
