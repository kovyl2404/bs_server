#! /bin/bash

PACKAGE_VER=1.0.0-1
PACKAGE_NAME=battleship-server

if [ ! -d "../../release/battleship-server" ]; then
	echo "Server release no found. Run 'make release' in project root first"
	exit 1
fi

mkdir -p ./$PACKAGE_NAME/opt
mkdir -p ./$PACKAGE_NAME/etc/battleship-server/

cp -r ../../release/battleship-server/ ./$PACKAGE_NAME/opt
cp  ./$PACKAGE_NAME/opt/$PACKAGE_NAME/etc/* ./$PACKAGE_NAME/etc/battleship-server/

fakeroot dpkg-deb --build $PACKAGE_NAME ./${PACKAGE_NAME}_${PACKAGE_VER}_amd64.deb

