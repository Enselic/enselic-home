for f in `git grep -il apache | sort | uniq`; do
    total=$(wc -l $f | awk '{print$1}')
    header=15
    remain=$(echo $(($total-$header)))
    tail -n $remain $f > $f.tmp
    mv $f.tmp $f
done 
