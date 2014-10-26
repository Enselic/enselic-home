for f in `find . -name *.dart`; do
    echo "/*
 * Copyright 2010-2014 Martin Nordholts <enselic@gmail.com>
 * All rights reserved.
 */
" > $f.tmp
    cat $f >> $f.tmp
    mv $f.tmp $f
done 
