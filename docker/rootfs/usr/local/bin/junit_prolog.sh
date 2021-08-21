#!/bin/bash

export GLOB="$*"
#[ -z "$GLOB" ] && GLOB="*_01.*"
export TEST_STEM=Report-$(echo "${GLOB}-Test" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -e "s/[^a-Z0-9_]/-/g" -e "s/--/-/g" -e "s/-/-/g"  -e "s/--/-/g" )
echo "<!-- TEST_STEM=${TEST_STEM} -->"

[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)

echo -e "<!-- Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE $GLOB) -->"

mkdir -p $TESTING_TEMP/
cat /dev/null > $TESTING_TEMP/failures.ansi
cat /dev/null > $TESTING_TEMP/successes.ansi
cat /dev/null > $TESTING_TEMP/test_results.ansi

# Run tests for JUnit Results
( test_prolog.sh -k $GLOB ) | tee -a $TESTING_TEMP/test_results.ansi

# Generate JUnit Results
echo "<testsuites>" > ./$TEST_STEM-junit.xml
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" $TESTING_TEMP/test_results.ansi >> ./$TEST_STEM-junit.xml
echo "</testsuites>" >> ./$TEST_STEM-junit.xml

PATH=~/.npm-packages/bin:$PATH

echo "<!-- "
# Generate Html Reports
(cat $TESTING_TEMP/test_results.ansi | ansi2html.sh > ./$TEST_STEM-README.html)  ; /bin/true
#npm install -g junit-viewer
(junit-viewer --results=./$TEST_STEM-junit.xml --save=./$TEST_STEM-junit-viewer.html &> ./$TEST_STEM-junit-viewer.debug.html )  ; /bin/true
#npm install -g xunit-viewer
(xunit-viewer --results ./$TEST_STEM-junit.xml -o ./$TEST_STEM-xunit-viewer.html )  ; /bin/true
#pip3 install junit2html
(junit2html ./$TEST_STEM-junit.xml ./$TEST_STEM-junit.html) ; /bin/true
echo "TEST_STEM=${TEST_STEM}"
echo "-->"