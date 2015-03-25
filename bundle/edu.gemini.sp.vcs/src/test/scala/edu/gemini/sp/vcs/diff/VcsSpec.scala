package edu.gemini.sp.vcs.diff


import edu.gemini.sp.vcs.diff.VcsAction._
import edu.gemini.sp.vcs.diff.VcsFailure.NeedsUpdate
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.spModel.obscomp.SPNote
import edu.gemini.util.security.principal.ProgramPrincipal
import org.specs2.specification.Fragments


import scalaz._

class VcsSpec extends VcsSpecification {
  import TestEnv._

  "checkout" should {
    "fail if the indicated program doesn't exist remotely" in withVcs { env =>
      notFound(env.local.superStaffVcs.checkout(Q2, DummyPeer), Q2)
    }

    "fail if the user doesn't have access to the program" in withVcs { env =>
      env.remote.addNewProgram(Q2)
      forbidden(env.local.vcs(ProgramPrincipal(Q1)).checkout(Q2, DummyPeer))
    }

    "transfer a program from the remote database to the local database" in withVcs { env =>
      // make the new program and store it remotely
      val remoteQ2 = env.remote.addNewProgram(Q2)

      // run checkout on the local peer
      env.local.superStaffVcs.checkout(Q2, DummyPeer).unsafeRun

      val localQ2 = env.local.odb.lookupProgramByID(Q2)

      localQ2.getLifespanId must be_!=(remoteQ2.getLifespanId)
    }
  }

  "add" should {
    "fail if the indicated program doesn't exist locally" in withVcs { env =>
      notFound(env.local.superStaffVcs.add(Q2, DummyPeer), Q2)
    }

    "fail if the user doesn't have access to the program" in withVcs { env =>
      env.local.addNewProgram(Q2)
      forbidden(env.local.vcs(ProgramPrincipal(Q1)).add(Q2, DummyPeer))
    }

    "transfer a program from the local database to the remote database" in withVcs { env =>
      // make the new program and store it locally
      val localQ2 = env.local.addNewProgram(Q2)

      // run add to send it to the remote peer
      env.local.superStaffVcs.add(Q2, DummyPeer).unsafeRun

      val remoteQ2 = env.remote.odb.lookupProgramByID(Q2)

      localQ2.getLifespanId must be_!=(remoteQ2.getLifespanId)
    }
  }

  "pull" should {
    "fail if the indicated program doesn't exist locally" in withVcs { env =>
      env.remote.addNewProgram(Q2)
      notFound(env.local.superStaffVcs.pull(Q2, DummyPeer), Q2)
    }

    "fail if the indicated program doesn't exist remotely" in withVcs { env =>
      env.local.addNewProgram(Q2)
      notFound(env.local.superStaffVcs.pull(Q2, DummyPeer), Q2)
    }

    "fail if the user doesn't have access to the program" in withVcs { env =>
      forbidden(env.local.vcs(ProgramPrincipal(Q2)).pull(Q1, DummyPeer))
    }

    "fail if the indicated program has different keys locally vs remotely" in withVcs { env =>
      env.local.addNewProgram(Q2)
      env.remote.addNewProgram(Q2)
      idClash(env.local.superStaffVcs.pull(Q2, DummyPeer), Q2)
    }

    "do nothing if the local version is the same" in withVcs { env =>
      expect(env.local.superStaffVcs.pull(Q1, DummyPeer)) {
        case \/-(false) => ok("")
      }
    }

    "do nothing if the local version is newer" in withVcs { env =>
      env.local.progTitle = "The Myth of Sisyphus"

      expect(env.local.superStaffVcs.pull(Q1, DummyPeer)) {
        case \/-(false) => ok("")
      } and (env.local.progTitle must_== "The Myth of Sisyphus")
    }

    "merge the updates if the remote version is newer" in withVcs { env =>
      env.remote.progTitle = "The Myth of Sisyphus"

      expect(env.local.superStaffVcs.pull(Q1, DummyPeer)) {
        case \/-(true) => ok("")
      } and (env.local.progTitle must_== "The Myth of Sisyphus")
    }
  }

  "push" should {
    "fail if the indicated program doesn't exist locally" in withVcs { env =>
      env.remote.addNewProgram(Q2)
      notFound(env.local.superStaffVcs.push(Q2, DummyPeer), Q2)
    }

    "fail if the indicated program doesn't exist remotely" in withVcs { env =>
      env.local.addNewProgram(Q2)
      notFound(env.local.superStaffVcs.push(Q2, DummyPeer), Q2)
    }

    "fail if the user doesn't have access to the program" in withVcs { env =>
      forbidden(env.local.vcs(ProgramPrincipal(Q2)).push(Q1, DummyPeer))
    }

    "fail if the indicated program has different keys locally vs remotely" in withVcs { env =>
      env.local.addNewProgram(Q2)
      env.remote.addNewProgram(Q2)
      idClash(env.local.superStaffVcs.push(Q2, DummyPeer), Q2)
    }

    "do nothing if the local version is the same" in withVcs { env =>
      expect(env.local.superStaffVcs.push(Q1, DummyPeer)) {
        case \/-(false) => ok("")
      }
    }

    "fail with NeedsUpdate if the local version is older" in withVcs { env =>
      env.remote.progTitle = "The Myth of Sisyphus"

      expect(env.local.superStaffVcs.push(Q1, DummyPeer)) {
        case -\/(NeedsUpdate) => ok("")
      } and (env.local.progTitle must_== Title)
    }

    "merge the updates if the local version is newer" in withVcs { env =>
      env.local.progTitle = "The Myth of Sisyphus"

      expect(env.local.superStaffVcs.push(Q1, DummyPeer)) {
        case \/-(true) => ok("")
      } and (env.remote.progTitle must_== "The Myth of Sisyphus")
    }

    // TODO: pending tests with conflicts, which must be rejected
  }

  def syncFragments(name: String, syncMethod: (Vcs, SPProgramID) => VcsAction[ProgramLocationSet]): Fragments = {
    name should {
      "fail if the indicated program doesn't exist locally" in withVcs { env =>
        env.remote.addNewProgram(Q2)
        notFound(syncMethod(env.local.superStaffVcs, Q2), Q2)
      }

      "fail if the indicated program doesn't exist remotely" in withVcs { env =>
        env.local.addNewProgram(Q2)
        notFound(syncMethod(env.local.superStaffVcs, Q2), Q2)
      }

      "fail if the user doesn't have access to the program" in withVcs { env =>
        forbidden(syncMethod(env.local.vcs(ProgramPrincipal(Q2)), Q1))
      }

      "fail if the indicated program has different keys locally vs remotely" in withVcs { env =>
        env.local.addNewProgram(Q2)
        env.remote.addNewProgram(Q2)
        idClash(syncMethod(env.local.superStaffVcs, Q2), Q2)
      }

      "do nothing if both versions are the same" in withVcs { env =>
        expect(syncMethod(env.local.superStaffVcs, Q1)) {
          case \/-(ProgramLocation.Neither) => ok("")
        }
      }

      "merge the remote updates if the remote version is newer" in withVcs { env =>
        env.remote.progTitle = "The Myth of Sisyphus"

        expect(syncMethod(env.local.superStaffVcs, Q1)) {
          case \/-(ProgramLocation.LocalOnly) => ok("")
        } and (env.local.progTitle must_== "The Myth of Sisyphus")
      }

      "send the local updates if the local version is newer" in withVcs { env =>
        env.local.progTitle = "The Myth of Sisyphus"

        expect(syncMethod(env.local.superStaffVcs, Q1)) {
          case \/-(ProgramLocation.RemoteOnly) => ok("")
        } and (env.remote.progTitle must_== "The Myth of Sisyphus")
      }

      "merge local and remote updates if both have been modified" in withVcs { env =>
        val group = env.local.odb.getFactory.createGroup(env.local.prog, null)
        env.local.prog.addGroup(group)

        val note = env.remote.odb.getFactory.createObsComponent(env.remote.prog, SPNote.SP_TYPE, null)
        env.remote.prog.addObsComponent(note)

        expect(syncMethod(env.local.superStaffVcs, Q1)) {
          case \/-(ProgramLocation.Both) => ok("")
        } and (env.remote.prog.getGroups.get(0).getNodeKey must_== group.getNodeKey) and
          (env.local.prog.getObsComponents.get(0).getNodeKey must_== note.getNodeKey)
      }

    }
  }

  syncFragments("sync", (vcs, pid) => vcs.sync(pid, DummyPeer))
  syncFragments("retrySync", (vcs, pid) => vcs.retrySync(pid, DummyPeer, 10))

  // TODO: pending tests with conflicts, which must be rejected
  // TODO: not really testing the "retry" part of "retrySync"

}
