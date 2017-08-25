#!/bin/bash -ex

# this will install dependencies globally
# for use in a shared-home-directory environment
# where the installation should be local to the
# machine (i.e. "global" wrt the machine)
# rather rather than global wrt the home directory
# (i.e. local wrt the machine)

# TODO:
pushd idris-config/dependancies/testing
idris --clean *.ipkg
sudo idris --install *.ipkg
popd

pushd idris-config/dependancies/lightyear
idris --clean *.ipkg
sudo idris --install *.ipkg
popd

pushd idris-config/dependancies/containers
idris --clean *.ipkg
sudo idris --install *.ipkg
popd

pushd idris-config/dependancies/lightyear
idris --clean *.ipkg
sudo idris --install *.ipkg
popd

pushd idris-config
idris --clean *.ipkg
sudo idris --install *.ipkg
popd


