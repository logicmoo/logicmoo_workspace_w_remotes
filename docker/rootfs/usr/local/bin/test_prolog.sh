#!/bin/bash

SWIPL=swipl
if [ -z `which swipl` ]; then
    # default locations on OS X
    SWIPL=/Applications/SWI-Prolog.app/Contents/MacOS/swipl;
    if [ ! -e $SWIPL ]; then
        SWIPL=~/bin/swipl;
    fi
    if [ ! -e $SWIPL ]; then
        INFO PFC requires SWI-Prolog. Please download from http://www.swi-prolog.org/
        return 1 2>/dev/null ; exit 1
    fi
fi

OUTER_TEE=""
[ -t 1 ] && OUTER_TEE="1"

good_exit=7
exitcode=$good_exit

[ -z "${keep_going}" ] && export keep_going=""
[ "$*" == *"-k"* ] && export keep_going="-k"

runtime_testing=4
export next_cls=0
export on_complete=test_completed


if [ "$1" == "-k" ]; then
  keep_going="-k"
  runtime_testing=5
  shift
fi


[ -z "${TESTING_TEMP}" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)

#// For test_prolog  (no args)
 declare -a listOfNames=(
                        # // sanity tests
                           "*_01*.p*" "*_02*.p*"
                        # // full tests
                         "*_03*.p*" "*_04*.p*" "*_05*.p*" "*_06*.p*" "*_07*.p*"
						 "*_08*.p*" "*_09*.p*" "*_10*.p*" "*_11*.p*" "*_12*.p*"
                        # // feature tests
                        # "*_f01*.p*" "*_f02*.p*" "*_f03*.p*" "_f04*.p*" "*_f05*.p*" "*_f06*.p*" "*_f07*.p*" "*_f08*.p*" "*_f09*.p*" "*_f10*.p*" "*_f11*.p*" "*_f12*.p*"
) 

# kill old dead jobs
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null
#[ -z "${OUTER_TEE}" ] && echo "<!--" && cls && echo -e "\n-->"
kill -9  %1 %2 %6 %5 %4 %3 %2 %1 &>/dev/null

if [ $# -ne 0 ]
then
   listOfNames=( "$@" )
   if [ $# -eq 1 ]
   then
      [ -z "${OUTER_TEE}" ] && echo "<!-- on_complete=true -->"
      on_complete=test_completed
   else
      echo -e "" # [ -z "${OUTER_TEE}" ] && echo "<!--" && cls && echo -e "\n-->"
   fi
else
      echo -e ""
fi




export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*_01.*"

[ -z "${TEST_STEM}" ] && TEST_STEM=Report-$(echo "${GLOB}-Units" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -e "s/[^a-Z0-9_]/-/g" -e "s/--/-/g" -e "s/-/-/g"  -e "s/--/-/g" )

REPORT_STEM=$(echo "$TEST_STEM-${PWD}" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -e "s/[^a-Z0-9_]/-/g" -e "s/--/-/g" -e "s/_/-/g"  -e "s/--/-/g" )

JUNIT_TESTS_GLOBBED="${TESTING_TEMP}/${REPORT_STEM}-glob"

echo "" > $JUNIT_TESTS_GLOBBED
function JECHO {
 (echo -e "${*}\n") >> $JUNIT_TESTS_GLOBBED
}
function INFO {
 JECHO "<!-- ${*} -->"
 echo -e "${*}\n"
}

JECHO "<testsuite name=\"${REPORT_STEM}\">"
me="${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}"
INFO "Running Matching Tests: $me $keep_going ${listOfNames[*]}"

(
for ele2 in "${listOfNames[@]}"
  do
  	for ele in $ele2
	do
	  retry=1
	  while [ $retry == 1 ]
	   do
	    retry=0
        export RunTestFile=Run_TestFile.$(echo "${PWD}/${ele}" | sed -e "s/[.]/_/g" -e "s|/|_|g")
		  [[ "$ele" == *".ansi" ]] && continue
        [[ "$ele" == *".html" ]] && continue
        [[ "$ele" == *".xml" ]] && continue
        [[ "$ele" == *".sh" ]] && continue
        if [[ "$ele" == *".sh" && -x "$ele" ]]; then
         CMD="./${ele}"
        else
   		#// Runs the test -f .swiplrc
         #CMD="swipl -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"thread_create(['${ele}'],Id),thread_join(Id),$on_complete\" "
         CMD="swipl -g 'set_prolog_flag(runtime_testing,${runtime_testing})' -g \"(['${ele}'])\" -g \"$on_complete\" "
        fi

        export TEE_FILE=$TESTING_TEMP/CMD_LAST.ansi
        export TEE_FILE2=${TEE_FILE}.too
        ####JECHO "<system-out><![CDATA["
        INFO "${date} (cd $PWD ; $CMD)" > $TEE_FILE
        INFO "${date} (cd $PWD ; $CMD)" > $TEE_FILE2
        startTime=$(date +%s);
        ( eval $CMD ) 2>&1 | sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" | tee -a $TEE_FILE | tee $TEE_FILE2
        exitcode=${PIPESTATUS[0]}
        endTime=$(date +%s);
        totalTime=$(($endTime-$startTime));        
        ####JECHO "]]></system-out>"

        classname=$(echo `pwd` | sed -e 's|/|.|g')
        if [ $exitcode -eq $good_exit ]; then
			[ "${next_cls}" == 1 ] && cls && next_cls=0			
         JECHO "<testcase name=\"$RunTestFile\" classname='$classname' time='$totalTime'>"
           INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
           JECHO "<system-out><![CDATA[$(cat $TEE_FILE2)]]></system-out>\n"
         JECHO "</testcase>"
			continue
	     fi
        JECHO "<testcase name=\"$RunTestFile\" classname='$classname' time='$totalTime'>"
        JECHO " <failure message='FAILED: $0 ${keep_going} ${ele} (returned ${exitcode})'>"
           JECHO "<system-err><![CDATA[$(cat $TEE_FILE2)]]></system-err>\n"
        JECHO " </failure></testcase>"

        next_cls=0

      [ "$on_complete" == 'on_complete' ] && [ $exitcode -ne 7 ] && INFO "FAILED: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 7 ] && INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 0 ] && [ "$on_complete" == 'true' ] && INFO "SUCCESS: $0 ${keep_going} ${ele} (returned ${exitcode})"
      [ $exitcode -eq 6 ] && retry=1 && continue


		# // 2 -> 1
		if [ $exitcode -eq 2 ]; then
         [ "$keep_going" == "-k" ] && INFO "...keep going..." && continue
         INFO "...NOT keep going..."
         exit 1
      fi
		
		# // Not Abort
		[ $exitcode -ne 1 ] && [ "$keep_going" == "-k" ] && continue

		echo "<!-- Do you wish to continue? [y]es, [a]lways [Up/r]etry or [N]o: "
		read -sN1 -r -t 0.0001 k1
		export k1
		
		while true
		do
			read -r -sn1 ans
			[ "$ans" == "" ] && break;
			case $ans in
			    A) break;;
				B) break;;
				r) break;;
            a) break;;
				y) break;;
				n) break;;
				e) break;;
				E) break;;
            D) break;;
			esac
			INFO "ans=$ans"
		done

      INFO "ans=$ans"

      [ "$ans" == '' ] && [ $exitcode -eq 0 ] && [ "$on_complete" == 'true' ]  && retry=1 && continue

      [ "$ans" == '' ] && [ $exitcode -eq 7 ] && retry=1 && cls && continue  # 7 + enter

		[ "$ans" == 'y' ] && continue
      [ "$ans" == 'a' ] && KEEP_GOING=1 && continue
		[ "$ans" == 'B' ] && continue # down arrow
		[ "$ans" == 'A' ] && retry=1 && cls && continue  # up arrow
		[ "$ans" == 'r' ] && retry=1 && continue

		INFO "Exiting the script. Have a nice day!"
		return $exitcode 2>/dev/null ; exit $exitcode
	  done
	done
  done
  return $exitcode 2>/dev/null ; exit $exitcode
) 

JECHO "</testsuite>\n\n\n\n"
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" $JUNIT_TESTS_GLOBBED > $JUNIT_TESTS_GLOBBED-junit.xml

