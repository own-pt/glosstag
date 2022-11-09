REL=~/hpsg/lkb_fos-src/src/tsdb/skeletons/english/Relations
pushd data
rm -rf own-??
for f in own-??.csv; do
    echo Processing $f
    delphin mkprof -vv -i $f --delimiter "@" --relations $REL --skeleton `basename $f .csv`;
done
popd

