VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here.  For a complete reference,
  # please see the online documentation at
  # https://docs.vagrantup.com/v2/

  config.vm.box = "ubuntu/trusty64"

  config.vm.post_up_message = <<-MSG
Welcome to the abs-to-haskell compiler.

Running the transcompiler tests:

cd /vagrant/test && ./test_compiler_codegen.sh

Compiling an ABS program:

a2h File1.abs File2.abs
# creates File1.hs, File2.hs haskell files, see also `a2h --help` for a list of compiler parameters

ghc --make -O File1.hs File2.hs -main-is File2
# creates a File2 binary executable with the entry point being the main block of File2

Running an ABS program:

./File2  # runs the program with default runtime parameters
./File2 --help  # lists the available runtime parameters without running the program
MSG

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 4096
    vb.cpus = 2
    vb.name = "abs2haskell_vagrant"
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"] # fix for ubuntu DNS problems
  end

  # Install necessary software
  config.vm.provision "shell",
                      privileged: false,
                      inline: <<-SHELL

sudo add-apt-repository ppa:hvr/ghc

echo
echo "Installing system updates"
echo

sudo apt-get update -y -q
sudo apt-get upgrade -y

echo
echo "Installing haskell tools"
sudo apt-get install -y -q ghc-7.8.4 cabal-install-1.20 happy-1.19.4 alex-3.1.3 git


# Set up paths
cat >/home/vagrant/.abstoolsrc <<EOF
PATH=\$PATH:/opt/ghc/7.8.4/bin:/opt/cabal/1.20/bin:/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:/vagrant/.cabal-sandbox/bin
export GHC_PACKAGE_PATH=/vagrant/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d:/opt/ghc/7.8.4/lib/ghc-7.8.4/package.conf.d:/home/vagrant/.ghc/x86_64-linux-7.8.4/package.conf.d
EOF

if [ -z "$(grep abstoolsrc /home/vagrant/.bashrc)" ] ; then
cat >>/home/vagrant/.bashrc <<EOF
. .abstoolsrc
EOF
fi

echo
echo "Building the ABS-Haskell compiler"
echo

cd /vagrant
git submodule init
git submodule update

PATH=$PATH:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin # necessary for building
ghc-pkg init /home/vagrant/.ghc/x86_64-linux-7.8.4/package.conf.d || true  # initialize the user ghc-db if missing
cabal sandbox init
cabal update
cabal sandbox add-source haxr-browser
cabal sandbox add-source opennebula
cabal install --only-dependencies
cabal install

  SHELL
end

