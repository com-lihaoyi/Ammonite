#!/usr/bin/env python
import os
from subprocess import check_call, check_output
import json
import sys

print "START TIME", datetime.datetime.now().isoformat()
is_master_commit = (
    os.environ['TRAVIS_PULL_REQUEST'] == "false" and
    os.environ['TRAVIS_BRANCH'] == "master"
)

all_versions = [
    "2.10.4", "2.10.5", "2.10.6",
    "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8"
]

def update_version():
    git_hash = check_output(["git", "rev-parse", "--short", "HEAD"]).strip()
    version_txt = """
        package ammonite
        object Constants{
          val version = "COMMIT-%s"
          val curlUrl = "https://git.io/vKwA8"
        }
    """ % git_hash

    open("project/Constants.scala", "w").write(version_txt)


def publish_signed():
    creds = """
        (credentials in ThisBuild) += Credentials("Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            "%s",
            "%s"
        )
        pgpPassphrase := Some("%s".toArray)
        pgpSecretRing := file("secring.asc")
        pgpPublicRing := file("pubring.asc")
        sonatypeProfileName := "com.lihaoyi"
    """ % (
        os.environ['SONATYPE_DEPLOY_USER'],
        os.environ['SONATYPE_DEPLOY_PASSWORD'],
        os.environ['SONATYPE_PGP_PASSWORD']
    )
    open("sonatype.sbt", "w").write(creds)
    open("secring.asc", "w").write(
        json.loads('"' + os.environ['SONATYPE_PGP_KEY_CONTENTS'] + '"')
    )
    open("pubring.asc", "w").write(
        json.loads('"' + os.environ['SONATYPE_PGP_PUB_KEY_CONTENTS'] + '"')
    )
    for version in all_versions:
        if version in {"2.10.5", "2.11.8"}:
            check_call(["sbt", "++"+version, "published/publishSigned"])
        else:
            check_call(["sbt", "++"+version, "amm/publishSigned", "sshd/publishSigned"])

    check_call(["sbt", "sonatypeReleaseAll"])


def publish_docs():
    deploy_key = json.loads('"' + os.environ['DEPLOY_KEY'] + '"')
    with open("deploy_key", "w") as f:
        f.write(deploy_key)
    if os.environ.get("TRAVIS_TAG"):
        new_env = dict(os.environ, DOC_FOLDER=".")
    else:
        new_env = dict(os.environ, DOC_FOLDER="master")
    check_call("ci/deploy_master_docs.sh", env=new_env)

if sys.argv[1] == "docs":
    if is_master_commit:
        print "MASTER COMMIT: Updating version and publishing to Github Pages"
        update_version()
        publish_docs()
    else:
        print "MISC COMMIT: Building readme for verification"
        check_call(["sbt", "readme/run"])

elif sys.argv[1] == "artifacts":
    if is_master_commit:
        print "MASTER COMMIT: Updating version and publishing to Maven Central"
        update_version()
        publish_signed()
    else:
        print "MISC COMMIT: Compiling all Scala code across versions for verification"
        for version in all_versions:
            check_call(["sbt", "++" + version, "published/compile"])

elif sys.argv[1] == "test":
    check_call(["sbt", "++" + os.environ["TRAVIS_SCALA_VERSION"], "published/compile"])
    check_call(["sbt", "++" + os.environ["TRAVIS_SCALA_VERSION"], sys.argv[2]])

else:
    raise Exception("Unknown argument list %s" % sys.argv)

print "END TIME", datetime.datetime.now().isoformat()
