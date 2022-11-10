REL=~/hpsg/lkb_fos-src/src/tsdb/skeletons/english/Relations
for f in data/own-??.csv; do
    echo Processing $f
    delphin mkprof -vv -i $f --delimiter "@" --relations $REL --skeleton data/$(basename $f .csv);
done

