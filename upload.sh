#!/bin/sh

echo "START BUILDING..."
stack build
stack exec site clean
stack exec site build
tmpdir=$(mktemp -d)
rsync -vr _site/* $tmpdir
git checkout master
rsync -vr $tmpdir/* .
echo ""
echo "START UPLOADING..."
echo ""
git push origin master
git checkout source
echo ""
echo "SUCCESS!"
