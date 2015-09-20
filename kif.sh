#!/bin/bash

# Kerning Input File compiler for TrueType AAT
# Copyright 2013 Grzegorz Rolek


usage=$(cat <<EOF
usage: $(basename $0) [-d] [-l] [-n] [-p post.xml] [-a anchors.kif] kerning.kif font.ttf
EOF
)


# UTILITY FUNCTIONS

# err message: print message to stderr and exit
err () {
	echo >&2 $0: "fatal: $1"
	exit ${2-1}
}

# indexof token list: find $index of token within list
indexof () {
	local token=$1; shift
	index=0
	for item
	do
		test $token = $item && return
		let index++
	done
	index=-1
}


# READING/PARSING FUNCTIONS

# readline: read next non-blank $line as list of tokens, comments excluded
readline () {
	test "$1" = '-b' && # -b: don't skip blank lines
		local blank='yes'

	unset line

	read || return; let lineno++

	# Skip comments as well as blank lines if not requested otherwise
	while test "$REPLY" -a -z "${REPLY##\/\/*}" -o \
		! "$blank" -a -z "${REPLY//[ 	]/}"
	do read || return; let lineno++
	done

	line=(${REPLY%%[ 	]\/\/*})

	# Include line indent as a null string in front of other tokens
	test "${REPLY//[ 	]/}" -a -z "${REPLY##[ 	]*}" &&
		line=("" ${line[@]})

	return 0 # mask status code of the test above
}

# clread: read classes into $clnames and $luarr until indented line
clread () {
	if test "$1" = '-u' # -u term: read until term line instead
	then
		local term="$2"
		shift 2
	fi

	clnames=($@) # Class names, with the ones provided
	clcount=$# # Number of classes

	luarr=() # glyph-to-class index lookup array
	unset lustart luend # first, last glyph with a class assigned

	# Read classes until either the given term line or line with an indent
	until test "$term" -a "$term" = "${line[*]}" -o -z "$line"
	do
		if test "$line" != '+'
		then
			clcount=${#clnames[@]} # temporarily current class index
			clnames+=($line)
		fi

		for glyph in ${line[@]:1}
		do
			index=$(grep -n "\"$glyph\"" $post | cut -d : -f 1)
			test $index ||
				err "glyph not found: $glyph (line $lineno)"
			let index-=postoff
			luarr[index]=$clcount
			test $index -lt ${lustart=$index} &&
				lustart=$index
			test $index -gt ${luend=$index} &&
				luend=$index
		done

		readline || return
	done

	# Set first, last glyph fallbacks to avoid iterating empty array
	lustart=${lustart-0}
	luend=${luend--1}

	let clcount++ # target class count
}


# PROCESSING FUNCTIONS

# bscalc: calculate bsearch header's $bsrange $bssel $bsshift
bscalc () {
	local usize=$1 nunits=$2
	bssel=0 # i.e., an exponent
	while test $(( nunits >> bssel )) -gt 1
	do let bssel++
	done
	local max=$(( 2**bssel ))
	test $max -eq 1 &&
		max=0 # bssel zero, no values in lookup
	bsrange=$(( usize * max ))
	bsshift=$(( usize * (nunits - max) ))
}

# lucollapse: filter out segments from $luarr into $lusegs
lucollapse () {
	lusegs=() # lookup segments
	lusegcount=0
	local i=$lustart; while test $i -le $luend
	do
		unset value; local value=${luarr[i]}

		# Skip slots with no value set
		if test -z "$value"
		then let i++; continue
		fi

		local first=$i

		# Skip each subsequent slot with same value
		while test -n "${luarr[i+1]}" &&
			test ${luarr[i+1]} -eq $value
		do let i++
		done

		local segment=($i $first $value)
		lusegs+=("${segment[*]}")
		let i++ lusegcount++
	done
}


# DATA-PRINTING FUNCTIONS

# printd: print data with format, length, comment, and values as appropriate
printd () {
	local form=$1 len=$2 comm=$3; shift 3

	test ! $debug &&
		unset comm

	test -n "$comm" &&
		comm=" <!-- $comm -->"

	printf "\t<dataline offset=\"%08X\" hex=\"" $off
	printf "$form" "$@"
	printf "\"/>$comm\n"

	let off+=len
}

# luprint array: print lookups from either $lusegs or array
luprint () {
	local arrname=$1; test $arrname &&
		eval local arr=(\${$arrname[@]}) # values to use instead

	# Calculate the bsearch header from unit size and count.
	bscalc $lumapsize $lusegcount

	printf "\n"

	printd "%04X" 2 "Lookup format" 2
	printd "%04X" 2 "Unit size" $lumapsize
	printd "%04X" 2 "No. of units" $lusegcount
	printd "%04X" 2 "Search range"  $bsrange
	printd "%04X" 2 "Entry selector" $bssel
	printd "%04X" 2 "Range shift" $bsshift

	for i in ${!lusegs[@]}
	do
		local s=(${lusegs[i]})
		test $debug &&
			local names="${glnames[s[0]]} - ${glnames[s[1]]}: ${clnames[s[2]]}"
		printd "%04X %04X %04X" $lumapsize "$names" ${s[@]:0:2} ${arr[i]-${s[2]}}
	done
	printd "%04X %04X %04X" $lumapsize "Guardian value" $(( 16#FFFF )) $(( 16#FFFF ))
}

# pad len: print zeros for byte-length of len
pad () {
	local len=$1

	test $len -ne 0 &&
		printd "%0*X" $len "" $(( len * 2 )) 0
}


# INITIAL SETUP

tag='kerx'; ver=2 # Table version, 'kerx' by default

# Reset and parse arguments
args=$(getopt np:a:ld $*)
test $? -ne 0 &&
	echo >&2 "$usage" && exit 2
set -- $args

for i
do
	case "$i" in
		-n) dry='yes'; shift;;
		-p) post=$2; shift 2;;
		-a) ankfile=$2; shift 2;;
		-l) tag='kern'; ver=1; shift;;
		-d) debug='yes'; shift;;
		--) shift; break;;
	esac
done

kif=$1; ttf=$2

# Make sure we have the right set of arguments
test -z "$kif" -o -z "$post" -a -z "$ttf" &&
	echo >&2 "$usage" && exit 2

# Set up paths for kerx.xml and ankr.xml data files
kerx="$(dirname "$ttf")/$tag.xml"
test $tag = 'kerx' -a "$ankfile" &&
	ankr="$(dirname "$ttf")/ankr.xml"

# Dump the 'post' table if necessary
if test -z "$post" -a "$ttf"
then
	post="$(dirname $ttf)/post.xml"
	type ftxdumperfuser &>/dev/null ||
		err "could not find ftxdumperfuser"
	ftxdumperfuser -t post -o $post $ttf
fi

# Find out how many lines need to be offset for a 'post' dump-based glyph lookup
postoff=$(grep -n '\.notdef' $post | cut -d : -f 1)
test $postoff ||
	err ".notdef glyph missing"

# Parse the 'post' dump for a list of glyphs names for debugging
test $debug &&
	glnames=($(sed -n 's/<PostScriptName ..* NameString=\"\(..*\)\".*>/\1/p' <$post))

eof="premature end of file"


# ANCHOR DATA PARSER/ASSEMBLER

if test $ankfile
then

	{

	printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
	printf "<genericSFNTTable tag=\"ankr\">\n"

	off=0 # current offset

	printd "%04X" 2 "Table version" 0
	printd "%04X" 2 "Flags" 0

	clrefs=() # Class names as referenced (vs. as defined)
	anchors=() # Anchor coordinates
	ankindices=() # Indices of anchors that start each new class
	ankoffsets=() # Calculated anchor offsets

	line=() # last line read as list of tokens
	lineno=0 # number of last line read

	# Read first line
	readline

	# Read classes until anchor listing header
	clread -u 'AnchorList'

	# Read the first class reference
	readline

	# Prevent the class reference from being mistakenly indented
	test $? -eq 0 -a -z "$line" &&
		err "class reference expected (line $lineno)"

	# Read anchor data until end of file
	loc=0; while test "$line"
	do
		indexof $line ${clnames[@]}
		test $index -eq -1 &&
			err "class not found: $line (line $lineno)"
		indexof $line ${clrefs[@]}
		test $index -ne -1 &&
			err "class referenced twice: $line (line $lineno)"
		clrefs+=($line)
		ankindices+=($loc)

		readline || break

		# Read coordinates in lines indented beneath the class reference
		while test -z "$line"
		do
			test $(( ( ${#line[@]} - 1 ) % 2 )) -ne 0 &&
				err "non-even number of coordinates (line $lineno)"
			anchors+=(${line[@]})

			let loc++

			readline || break 2
		done
	done

	# Resolve class indices as defined into order of their reference
	i=$lustart; while test $i -le $luend
	do
		# Skip unset slots or they'll default to value zero
		if test -z "${luarr[i]}"
		then let i++; continue
		fi

		name=${clnames[luarr[i]]}
		indexof $name ${clrefs[@]}

		# Remove unused classes from lookup array, if any.
		test $index -eq -1 &&
			index=""

		luarr[i++]=$index
	done

	# Use only the referenced class names from now on.
	clnames=(${clrefs[@]}); unset clrefs

	# Filter out ranges of glyphs with same class from the lookup array
	lucollapse

	luoff=12 # lookup table offset
	luhead=12 # binsearch lookup header size
	lusize=2 # lookup value size
	lumapsize=$(( 4 + lusize )) # single mapping size
	lumapcount=$(( lusegcount + 1 ))
	lulen=$(( luhead + lumapcount * lumapsize ))
	lupad=$(( lulen / 2 % 2 * 2 ))
	ankoff=$(( luoff + lulen + lupad ))

	printd "%08X" 4 "Anchor lookup offset" $luoff
	printd "%08X" 4 "Anchors offset" $ankoff

	# Translate anchor indices to offsets.
	for i in ${!lusegs[@]}
	do
		s=(${lusegs[i]})
		ankoffsets+=($(( ankindices[s[2]] * 4 + s[2] * 4 )))
	done

	# Print the lookup array, but with offsets instead of indices.
	luprint 'ankoffsets'

	pad $lupad

	nanchors=$(( ${#anchors[@]} / 2 ))
	printf "\n"
	v=0; for i in ${!ankindices[@]}
	do
		nextank=${ankindices[i+1]=$nanchors}
		printd "%08X" 4 "${clnames[i]}" $(( nextank - v/2 ))

		while test $(( v / 2 )) -lt $nextank
		do
			xval=${anchors[v++]}
			yval=${anchors[v++]}

			test $xval -lt 0 &&
				let xval+=16#10000 # 2's complement of sorts
			test $yval -lt 0 &&
				let yval+=16#10000

			printd "%04X %04X" 4 "" $xval $yval
		done
	done

	printf "</genericSFNTTable>\n"

	} <$ankfile >$ankr

fi


# MAIN KERNING FILE PARSER/ASSEMBLER

{

printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
printf "<genericSFNTTable tag=\"%s\">\n" $tag

off=0 # current offset

printd "%04X%04X" 4 "Table version" $ver 0
printd "%08X" 4 "No. of subtables" $(grep -c '^Type[ 	]' $kif)

tbhead=$(( 4 + 2 * 2*ver )) # subtable header size
luoff=$(( 5 * 2*ver )) # lookup table offset (length of state table header)
lusize=$ver # lookup value size
trsize=$ver # transition entry size
etsize=$(( 2 + 2*ver )) # full entry size
vlsize=2 # value size

line=() # last line read as list of tokens
lineno=0 # number of last line read

# Read first line of input
readline

# Read the file subtable by subtable
while test "$line" = 'Type'
do

	flvert='no'
	flcross='no'
	unset tbfmt acttype # Subtable format and action type
	states=() # State records
	stnames=() # State names
	gotos=() # Next states
	gtnames=() # Names of the next states
	flpush=() # Flags for the push action
	fladvance=() # Flags for the advance action
	actions=() # Kern value lists to apply
	actnames=() # Names of kern value lists to apply
	actlines=() # Line numbers of actions for reporting undefined values
	values=() # Kerning values
	vlnames=() # Names of the kern value lists
	vlindices=() # Indexes of values beginning the lists
	eolmarks=() # Number of end-of-list markers prior to each list
	eolmarkcount=0 # End-of-list marker count in total
	vlpack=1 # Number of values per record; depends on table format

	test "$line" != 'Type' &&
		err "kerning type expected (line $lineno)"
	case ${line[@]:1} in
		Contextual) tbfmt=1;;
		Attachment) tbfmt=4;;
		*) err "unknown kerning type: ${line[@]:1} (line $lineno)";;
	esac

	test $tbfmt -eq 4 -a $tag != 'kerx' &&
		err "attachment allowed in 'kerx' table only (line $lineno)"

	readline || err "$eof (line $lineno)"

	test "$line" != 'Orientation' &&
		err "kerning orientation expected (line $lineno)"
	case ${line[@]:1} in
		V) flvert='yes';;
		H) ;;
		*) err "bad orientation flag: ${line[@]:1} (line $lineno)";;
	esac

	readline || err "$eof (line $lineno)"

	if test "$line" = 'Cross-stream'
	then
		case ${line[@]:1} in
			yes) flcross='yes';;
			no) ;;
			*) err "bad cross-stream flag: ${line[@]:1} (line $lineno)";;
		esac

		readline || err "$eof (line $lineno)"
	fi

	# Read classes until state table header, the four default classes included
	clread 'EOT' 'OOB' 'DEL' 'EOL' || err "$eof (line $lineno)"

	# Check if classes in class listing and state table header match
	test "${clnames[*]}" != "${line[*]:1}" &&
		err "classes and state header don't match (line $lineno)"

	# Read the first state, skipping any blank lines directly beneath the header
	readline || err "$eof (line $lineno)"

	# Read the state table until either a blank line or an indented one
	until test -z "${line[*]}" -o -z "$line"
	do
		stnames+=($line)

		state=()
		for entry in ${line[@]:1}
		do
			test $entry -le 0 &&
				err "non-positive entry number (line $lineno)"

			state+=($(( --entry ))) # zero-based indices
		done

		test "${#state[@]}" -ne "$clcount" &&
			err "wrong entry count in state: $line (line $lineno)"
		states+=("${state[*]}")

		readline -b || err "$eof (line $lineno)"
	done

	# Skip additional blank lines if necessary
	if test -z "${line[*]}"
	then readline || err "$eof (line $lineno)"
	fi

	# See if entry table header has one of the expected forms
	if test $tag = 'kerx' -a $tbfmt -eq 4
	then
		case "${line[*]:1}" in
			"GoTo Mark? Advance? MatchPoints") acttype=1; vlpack=2;;
			"GoTo Mark? Advance? MatchAnchors") acttype=2; vlpack=2;;
			"GoTo Mark? Advance? MatchCoords") acttype=4; vlpack=4;;
			*) err "malformed entry table header (line $lineno)";;
		esac
	else
		test "${line[*]:1}" != "GoTo Push? Advance? KernValues" &&
			err "malformed entry table header (line $lineno)"
	fi

	# Read the first entry, skipping blank lines beneath the header
	readline || err "$eof (line $lineno)"

	# Read entries until a blank line, or an indented one (the Font Tools way)
	until test -z "${line[*]}" -o -z "$line"
	do
		entry=$(( ${#gotos[@]} + 1 ))
		test $line -eq $entry ||
			err "non-sequential entry number: $line (line $lineno)"

		stname=${line[@]:1:1}
		indexof $stname ${stnames[@]}
		test $index -eq -1 &&
			err "state not found: $stname (line $lineno)"
		gotos+=($index)
		gtnames+=($stname)

		flpush+=(${line[@]:2:1})
		fladvance+=(${line[@]:3:1})
		actnames+=(${line[@]:4:1})
		actlines+=($lineno)

		readline -b || err "$eof (line $lineno)"
	done

	if test -z "${line[*]}"
	then readline || err "$eof (line $lineno)"
	fi

	# Read values until either next subtable header or end of file
	until test "$line" = 'Type'
	do
		vlnames+=(${line:-${line[1]}}) # in case of indent on first name
		vlindices+=(${#values[@]})

		readline || break

		if test $tbfmt -eq 4
		then
			# Expect specific value count in each attachment type.
			case $acttype in
				1) vlnreq=1;; # point per glyph
				2) vlnreq=1;; # anchor
				4) vlnreq=2;; # pair of coordinates
			esac

			# Read values for both marked and current glyphs.
			for field in Marked Current
			do
				test "${line[1]}" != "${field}" &&
					err "values for $field glyph expected (line $lineno)"
				test $(( ${#line[@]} - 2 )) -ne $vlnreq &&
					err "wrong number of values (line $lineno)"

				value=${line[@]:2}

				if test $acttype -eq 1 -o $acttype -eq 2
				then
					test $value -le 0 &&
						err "non-positive index value (line $lineno)"

					let value-- # zero-based indices
				fi

				values+=($value)

				readline || if test $field = 'Marked'
					then err "$eof (line $lineno)"
					else break
					fi
			done

			# Don't bother with other formats at this point.
			continue
		fi

		# Read values in lines indented beneath the name
		while test -z "$line"
		do
			# Fail on Reset value in a non-cross-stream table
			test $flcross != 'yes' &&
				for value in ${line[@]}
				do test $value = 'Reset' &&
					err "kern reset in a non-cross-stream table (line $lineno)"
				done

			values+=(${line[@]})

			readline || break 2
		done
	done

	# Now with the values parsed match their indices to actions.
	for i in ${!actnames[@]}
	do
		name=${actnames[i]}
		action=-1

		if test $name != 'none'
		then
			indexof $name ${vlnames[@]}
			test $index -eq -1 &&
				err "kern values not defined: $name (line ${actlines[i]})"
			action=$index
		fi

		actions+=($action)
	done

	# Pre-compute the end-of-list marker count prior to each action.
	if test $tag = 'kerx' -a $tbfmt -eq 1
	then
		prev=0; nmarks=0; for i in ${!vlindices[@]}
		do
			curr=${vlindices[i]}

			# Increase the count for non-empty actions only.
			test $curr -gt $prev &&
				let nmarks++

			eolmarks+=($nmarks)
			prev=$curr
		done

		eolmarkcount=$(( nmarks + 1 ))
	fi

	if test $tag = 'kerx'
	then
		# Filter out glyph ranges of same class
		lucollapse

		luhead=12 # segmented lookup table header size
		lumapsize=$(( 4 + lusize ))
		lumapcount=$(( lusegcount + 1 ))
	else
		luhead=4 # trimmed lookup array header size
		lumapsize=$lusize
		lumapcount=$(( luend - lustart + 1 ))
	fi

	lulen=$(( luhead + lumapcount * lumapsize ))
	lupad=$(( lulen/ver % 2 *ver ))
	stoff=$(( luoff + lulen + lupad ))
	stlen=$(( ${#states[@]} * clcount * trsize ))
	stpad=$(( stlen/ver % 2 *ver ))
	etoff=$(( stoff + stlen + stpad ))
	etlen=$(( ${#gotos[@]} * etsize ))
	etpad=$(( etlen/ver % 2 *ver ))
	vloff=$(( etoff + etlen + etpad ))
	vllen=$(( ${#values[@]} * vlsize ))
	(( vllen += eolmarkcount * vlsize )) # end-of-list markers, if any
	vlpad=$(( vllen/ver % 2 *ver ))
	tblen=$(( tbhead + vloff + vllen + vlpad ))

	printf "\n"
	if test $debug
	then
		printf "\t<!-- Subtable No. %d -->\n" $(( ++tbno ))
		printf "\n"
	fi

	flags=0
	test $flvert = 'yes' && let flags+=16#80
	test $flcross = 'yes' && let flags+=16#40

	test $tag = 'kerx' &&
		(( flags <<= 16 )) # extended coverage field

	printd "%08X" 4 "Subtable length" $tblen
	printd "%0*X" $(( 2*ver - 1 )) "Coverage" $(( 4*ver - 2 )) $flags
	printd "%02X" 1 "Format" $tbfmt
	printd "%0*X" $(( 2*ver )) "Variation tuple index" $(( 4*ver )) 0

	test $acttype &&
		vloff=$(( vloff + (acttype << 29) )) # first two bits

	printf "\n"

	printd "%0*X" $(( 2*ver )) "Class count" $(( 4*ver )) $clcount
	printd "%0*X" $(( 2*ver )) "Class lookup offset" $(( 4*ver )) $luoff
	printd "%0*X" $(( 2*ver )) "State table offset" $(( 4*ver )) $stoff
	printd "%0*X" $(( 2*ver )) "Entry table offset" $(( 4*ver )) $etoff
	printd "%0*X" $(( 2*ver )) "Values offset" $(( 4*ver )) $vloff

	# Print either the modern or the legacy lookup array.
	if test $tag = 'kerx'
	then
		luprint
	else
		printf "\n"

		printd "%04X" 2 "First glyph" $lustart
		printd "%04X" 2 "Glyph count" $lumapcount

		i=$lustart; while test $i -le $luend
		do
			class=${luarr[i]-1}
			test $debug &&
				names="${glnames[i]}: ${clnames[class]}"
			printd "%02X" $lumapsize "$names" $class
			let i++
		done
	fi

	pad $lupad

	printf "\n"
	if test $debug
	then
		# Print at least stubs of class names along the transitions
		printf "\t                            <!-- "
		for name in ${clnames[@]}
		do printf "%-*.*s " $(( 2*ver )) $(( 2*ver )) $name
		done
		printf " -->\n"
	fi

	for i in ${!states[@]}
	do
		printd "%0*X " $(( trsize * clcount )) "${stnames[i]}" $(

			for trans in ${states[i]}
			do echo $(( trsize * 2 )) $trans
			done

			)
	done

	pad $stpad

	printf "\n"
	for i in ${!gotos[@]}
	do
		goto=${gotos[i]}
		action=${actions[i]}

		flags=0
		test ${flpush[i]} = 'yes' && let flags+=16#8000
		test ${fladvance[i]} = 'yes' || let flags+=16#4000

		test $debug &&
			comment="$(printf "%02X" $i) ${gtnames[i]}"

		if test $tag = 'kerx'
		then
			if test $action -ge 0
			then
				vlindex=${vlindices[action]}
				if test $tbfmt -eq 4
				then
					let vlindex/=vlpack
				else
					let vlindex+=eolmarks[action]
				fi
			else
				let vlindex=16#FFFF
			fi

			printd "%04X %04X %04X" $etsize "$comment" $goto $flags $vlindex
		else
			goto=$(( stoff + goto * clcount )) # byte-offset

			test $action -ge 0 &&
				(( flags += vloff + vlindices[action] * vlsize ))

			printd "%04X %04X" $etsize "$comment" $goto $flags
		fi
	done

	pad $etpad

	printf "\n"
	v=0; for i in ${!vlindices[@]}
	do
		nextval=${vlindices[i+1]=${#values[@]}}
		count=$(( nextval - vlindices[i] ))
		len=$(( count * vlsize ))
		test $tag = 'kerx' -a $tbfmt -eq 1 &&
			let len+=vlsize # end-of-list value

		printd "%04X " $len "${vlnames[i]}" $(

			w=$v; while test $w -lt $nextval
			do
				value=${values[w]}

				test $value = 'Reset' &&
					let value=16#8000 # cross-stream reset flag

				test $value -lt 0 &&
					let value+=16#10000 # 2\'s complement of sorts

				if test $tag != 'kerx'
				then
					# Clear the least significant bit
					(( value -= value % 2 ))

					test $(( w + 1 )) -eq $nextval &&
						let value+=1 # end-of-list flag
				fi

				echo $value

				test $tag = 'kerx' -a $tbfmt -eq 1 -a $(( w + 1 )) -eq $nextval &&
					echo $(( 16#FFFF )) # end-of-list value

				let w++
			done

			)

		let v+=count
	done

	pad $vlpad

done

printf "</genericSFNTTable>\n"

} <$kif >$kerx


# FINAL FUSE

# Don't bother fusing if it's a dry run
test $dry && exit

type ftxdumperfuser &>/dev/null ||
	err "could not find ftxdumperfuser"

# Fuse assembled data files into the font
for data in $ankr $kerx
do ftxdumperfuser -g -t $(basename -s .xml $data) -d $data $ttf
done

