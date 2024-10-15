# Steps to follow for each release

## Finalise opam code for release
* update version in all the opam files and in configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* if you're releasing the first final release of a new branch (e.g. 2.2.0): make sure `root_version` in OpamFile.ml is set to the final release number (e.g. for 2.2.0, root_version should be 2.2). Make sure that opamFormatUpgrade.ml also contains an upgrade function from the previous version (that function will most likely be empty)
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fill it

## Windows setup

* From the "release" directory (where this here readme.md file should be)...
* Download Windows 10 English International 64bit from https://www.microsoft.com/en-gb/software-download/windows10ISO
* Run qemu-img create -f qcow2 Windows-10-x86_64.qcow2 32G
* Run qemu-system-x86_64 -cdrom Win10_22H2_EnglishInternational_x64v1.iso -drive file=Windows-10-x86_64.qcow2 -smp 8 -m 6G -net nic -net user,restrict=on -machine q35
* Install Windows. Everything stays as default except:
  * Click "I don’t have a product key"
  * Choose "Windows 10 Pro"
  * Click "Custom: ..." not "Upgrade: ..."
* This will take an hour or so
* Select the default keyboard then wait some more
* Upon restart:
  * no online account
  * yes, i want the limited experience
  * username: opam
  * password: password
  * 3 hints: just random or something
  * no location
  * no find my device
  * no improve inking & typing
  * send required diagnostic data
  * no advertising ID
  * no cortana
* Open gpedit.msc and right-click on "Computer Configuration -> Administrative Templates -> Windows Components -> Windows Update -> Configure Automatic Updates", click Edit and in the new window click on Enabled and then select "2. Notify for download and auto install", click Apply and then Ok.
* Start -> Power off
* Run qemu-system-x86_64 -drive file=Windows-10-x86_64.qcow2 -smp 8 -m 6G -machine q35
* Answer yes to the "would you like to be discoverable on the network" prompt
* Open the Settings app
  * "System -> Power & Sleep" and select "Never" on the "When plugged in, turn off after" drop-down menu
  * Put the slider on "Best performance"
  * search for "Add an optional feature", then search for "OpenSSH" and install "OpenSSH Server"
* Run services.msc
  * Double-click OpenSSH SSH Server then select Automatic from the Startup drop-down menu
  * Click Ok
* Shutdown the computer using the startmenu button

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -am 2.2.0 2.2.0; git push origin 2.2.0)
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* Make sure the repository is in the correct state: `git switch --detach <tag>`
* launch docker using the Docker GUI macOS app
* generate opam artifacts, using `release/release.sh <tag>` from a macOS/arm64 machine, it requires to have Docker and QEMU installed (see below device requirements)
* generate the signatures using `release/sign.sh <tag>`
* add releases notes (content of `master_changes.md`) in the release draft
* upload everything from `release/out/<tag>`
* finalise the release (publish)

## Publish the release

* add hashes in `install.sh` (and check signatures)
* bring the changes to the changelog (CHANGES) from the branch of the release to the `master` branch
* publish opam packages in opam-repository (and add `flags: avoid-version` and `available: opam-version >= "2.1.0"` to each packages if this is not a stable version)
* update versions (and messages, if necessary) in https://github.com/ocaml/opam-repository/blob/master/repo

## Announce!

* a blog entry in opam.ocaml.org
* a announcement in discuss.ocaml.org
* copy the blog entry from opam.ocaml.org for https://github.com/ocaml/ocaml.org/tree/main/data/changelog/opam
* announce the release on the OCaml Discord server


## After release

* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update

### On a release candidate
* create a branch to a `x.y` for rc's and the final release

---

## Device requirements
* Mac M1 or above with Rosetta2
* >=70GB of disk space free
* brew dependencies: git, gpg, qemu>=8.1.0 (avoid qemu 9.1.0, see https://gitlab.com/qemu-project/qemu/-/issues/2581), docker>=24.0.0, sshpass
* opam repo with the tag fetched
* Have the secret key available
