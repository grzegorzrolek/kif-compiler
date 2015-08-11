#!/bin/bash

# Kerning Input File compiler for TrueType AAT
# Copyright 2013 Grzegorz Rolek


usage=$(cat <<EOF
usage: $(basename $0) [-l] [-n] [-p post.xml] [-a anchors.kif] kerning.kif font.ttf
EOF
)


eof="fatal: premature end of file"

# err message: print message to stderr and exit
err () {
	echo >&2 $0: $1
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

tag='kerx'; ver=2 # Table version, 'kerx' by default

# Parse and reset the arguments.
args=$(getopt np:a:l $*)
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
		--) shift; break;;
	esac
done

kif=$1; ttf=$2

# Make sure we have the right set of arguments
test -z "$kif" -o -z "$post" -a -z "$ttf" &&
	echo >&2 "$usage" && exit 2

# Prepare the paths for the kerx.xml and ankr.xml data files.
kerx="$(dirname "$ttf")/$tag.xml"
test $tag = 'kerx' -a "$ankfile" &&
	ankr="$(dirname "$ttf")/ankr.xml"

# Dump the 'post' table, if not given explicitly.
if test -z "$post" -a "$ttf"
then
	post="$(dirname $ttf)/post.xml"
	type ftxdumperfuser &>/dev/null ||
		err "fatal: couldn't find the ftxdumperfuser"
	ftxdumperfuser -t post -o $post $ttf
fi

# Find the line offset to the first glyph name record in the 'post' dump.
postoff=$(grep -n '\.notdef' $post | cut -d : -f 1)
test $postoff ||
	err "fatal: required glyph .notdef missing"

# Parse the 'post' table dump for an array of glyphs names.
glnames=($(sed -n 's/<PostScriptName ..* NameString=\"\(..*\)\".*>/\1/p' <$post))

# clread term: read classes into $clnames and $luarr until term pattern
clread () {
	local term="$1"; shift # termination line pattern

	clnames=($@) # Class names, with the ones provided
	clcount=$# # Number of classes
	unset glstart glend # First/last glyph with a class assigned
	luarr=() # Glyph-to-class index lookup array

	# Read classes until a line with the termination pattern found.
	until test -z "${REPLY##$term}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		if test $line != '+'
		then
			clcount=${#clnames[@]}
			clnames=(${clnames[@]} $line)
		fi

		for glyph in ${line[@]:1}
		do
			index=$(grep -n "\"$glyph\"" $post | cut -d : -f 1)
			test $index ||
				err "fatal: glyph not found: $glyph (line $lineno)"
			let index-=$postoff
			luarr[$index]=$clcount
			test $index -lt ${glstart=$index} && glstart=$index
			test $index -gt ${glend=$index} && glend=$index
		done

		read || err "$eof"; let lineno++

		# Skip blanks and comments inbetween.
		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || err "$eof"; let lineno++
		done
	done

	# Update the number of classes.
	let clcount++
}

# bscalc: calculate bsearch header's $bsrange $bssel $bsshift
bscalc () {
	local usize=$1 nunits=$2
	bssel=0 # i.e., an exponent
	while test $(( nunits >> bssel )) -gt 1
	do let bssel++
	done
	bsrange=$(( usize * 2**bssel ))
	bsshift=$(( usize * (nunits - 2**bssel) ))
}

# lucollapse: filter out segments from $luarr into $lusegs
lucollapse () {
	local start=$1 end=$2
	lusegs=() # lookup segments
	lusegcount=0
	local i=$start; while test $i -le $end
	do
		unset value; local value=${luarr[$i]}

		# Skip holes in the array.
		if test -z "$value"
		then let i++; continue
		fi

		local first=$i

		# Skip to the last subsequent slot with same value.
		while test -n "${luarr[$((i+1))]}" &&
			test ${luarr[$((i+1))]} -eq $value
		do let i++
		done

		local segment=($i $first $value)
		lusegs[$lusegcount]=${segment[@]}
		let i++ lusegcount++
	done
}

# printd: print data with format, length, comment, and values as appropriate
printd () {
	local form=$1 len=$2 comm=$3; shift 3

	test -n "$comm" &&
		comm=" <!-- $comm -->"

	printf "\t<dataline offset=\"%08X\" hex=\"" $off
	printf "$form" "$@"
	printf "\"/>$comm\n"

	let off+=$len
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
		local s=(${lusegs[$i]})
		local names="${glnames[${s[0]}]} - ${glnames[${s[1]}]}: ${clnames[${s[2]}]}"
		printd "%04X %04X %04X" $lumapsize "$names" ${s[@]:0:2} ${arr[$i]-${s[2]}}
	done
	printd "%04X %04X %04X" $lumapsize "Guardian value" $(( 16#FFFF )) $(( 16#FFFF ))
}

# pad len: print zeros for byte-length of len
pad () {
	local len=$1

	test $len -ne 0 &&
		printd "%0*X" $len "" $(( len * 2 )) 0
}


# Parse the anchor input file if given.
if test $ankfile
then

	{

	lineno=0 # number of line being parsed
	off=0 # current offset

	clrefs=() # Class names as referenced (vs. as defined)
	anchors=() # Anchor coordinates
	ankindices=() # Indices of anchors that start each new class
	ankoffsets=() # Calculated anchor offsets

	# Read the first line.
	read; let lineno++

	# Skip blank lines and line comments in front of the input file.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || break; let lineno++
	done

	printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
	printf "<genericSFNTTable tag=\"ankr\">\n"

	printd "%04X" 2 "Table version" 0
	printd "%04X" 2 "Flags" 0

	# Read classes until the the anchor listing shows up.
	clread 'AnchorList*'

	# Read the first class reference.
	read || err "$eof"; let lineno++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	# Prevent the first reference from being accidentally indented.
	test -z "${REPLY##[ 	]*}" &&
		err "fatal: class reference expected (line $lineno)"

	# Read anchors until the end of file.
	let loc=0; until test -z "$REPLY"
	do
		line=(${REPLY%%[ 	]\/\/*})
		indexof $line ${clnames[@]}
		test $index -eq -1 &&
			err "fatal: class not found: $line (line $lineno)"
		indexof $line ${clrefs[@]}
		test $index -ne -1 &&
			err "fatal: class referenced twice: $line (line $lineno)"
		clrefs[${#clrefs[@]}]=$line
		ankindices=(${ankindices[@]} $loc)

		read || break; let lineno++

		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || break 2; let lineno++
		done

		# Read coordinates in the indented lines beneath.
		while test -z "${REPLY##[ 	]*}"
		do
			line=(${REPLY%%[ 	]\/\/*})
			test $(( ${#line[@]} % 2 )) -ne 0 &&
				err "fatal: wrong number of coordinates (line $lineno)"
			anchors=(${anchors[@]} ${line[@]})

			let loc++

			read || break 2; let lineno++

			until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
			do read || break 3; let lineno++
			done
		done
	done

	# Resolve classes as defined into classes as referenced.
	let i=$glstart; while test $i -le $glend
	do
		# Skip null values or they'll default to zero.
		if test -z "${luarr[$i]}"
		then let i++; continue
		fi

		name=${clnames[${luarr[$i]}]}
		indexof $name ${clrefs[@]}

		# Remove unused classes from lookup array, if any.
		test $index -eq -1 &&
			index=""

		luarr[$i]=$index
		let i++
	done

	# Use only the referenced class names from now on.
	clnames=(${clrefs[@]}); unset clrefs

	# Filter out segments from the lookup array.
	lucollapse $glstart $glend

	let luoff=12 # Lookup table offset, constant
	let luhead=6*2 # Size of a bsearch lookup table header
	let lusize=2 # Size of the anchor offset lookup entry
	let lumapsize=2*2+$lusize # Mapping size
	let lumapcount=$lusegcount+1 # Number of mappings
	let lulen=$luhead+$lumapcount*$lumapsize # Lookup table length
	let lupad=$lulen/2%2*2 # Padding
	let ankoff=$luoff+$lulen+$lupad # Anchor table offset

	printd "%08X" 4 "Anchor lookup offset" $luoff
	printd "%08X" 4 "Anchors offset" $ankoff

	# Translate anchor indices to offsets.
	for i in ${!lusegs[@]}
	do
		s=(${lusegs[$i]})
		let ankoffset=${ankindices[${s[2]}]}*4+${s[2]}*4
		ankoffsets[$i]=$ankoffset
	done

	# Print the lookup array, but with offsets instead of indices.
	luprint 'ankoffsets'

	pad $lupad

	let nanchors=${#anchors[@]}/2 # No. of all the anchors
	printf "\n"
	let v=0; for i in ${!ankindices[@]}
	do
		nextank=${ankindices[$(( i + 1 ))]=$nanchors}
		printd "%08X" 4 "${clnames[$i]}" $(( nextank - v/2 ))

		while test $(( v / 2 )) -lt $nextank
		do
			xval=${anchors[$v]}
			yval=${anchors[$(( v + 1 ))]}
			let v+=2

			# Make a 2's complement for a negative value.
			test $xval -lt 0 && let xval=16#10000+$xval
			test $yval -lt 0 && let yval=16#10000+$yval

			printd "%04X %04X" 4 "" $xval $yval
		done
	done

	printf "</genericSFNTTable>\n"

	} <$ankfile >$ankr

fi

{

lineno=0 # number of line being parsed
off=0 # current offset

# Read the first line.
read; let lineno++

# Skip blank lines and line comments in front of the input file.
until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
do read || break; let lineno++
done

printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
printf "<genericSFNTTable tag=\"%s\">\n" $tag

printd "%04X%04X" 4 "Table version" $ver 0
printd "%08X" 4 "No. of subtables" $(grep -c '^Type[ 	]' $kif)

let tbhead=4+2*2*$ver # Subtable header size
let luoff=5*2*$ver # Lookup table offset (length of a state table header)
let lusize=1*$ver # Size of the lookup value
let trsize=1*$ver # Transition size
let etsize=2+2*$ver # Full entry size in the entry table
let vlsize=2 # Value size

# Read the file subtable by subtable to the end of file.
while test "$REPLY" -a -z "${REPLY##Type[ 	]*}"
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

	# Parse the input line, comments excluded.
	line=(${REPLY%%[ 	]\/\/*})
	test $line != "Type" &&
		err "fatal: kerning type expected (line $lineno)"
	case ${line[@]:1} in
		Contextual) tbfmt=1;;
		Attachment) tbfmt=4;;
		*) err "fatal: unknown kerning type: ${line[@]:1} (line $lineno)";;
	esac

	test $tbfmt -eq 4 -a $tag != 'kerx' &&
		err "fatal: attachment allowed in 'kerx' table only (line $lineno)"

	read || err "$eof"; let lineno++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	line=(${REPLY%%[ 	]\/\/*})
	test $line != "Orientation" &&
		err "fatal: kerning orientation expected (line $lineno)"
	case ${line[@]:1} in
		V) flvert='yes';;
		H) ;;
		*) err "fatal: bad orientation flag: ${line[@]:1} (line $lineno)";;
	esac

	read || err "$eof"; let lineno++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	line=(${REPLY%%[ 	]\/\/*})
	if test $line = "Cross-stream"
	then
		case ${line[@]:1} in
			yes) flcross='yes';;
			no) ;;
			*) err "fatal: bad cross-stream flag: ${line[@]:1} (line $lineno)";;
		esac

		read || err "$eof"; let lineno++

		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || err "$eof"; let lineno++
		done
	fi

	# Read classes until a state table header (indented line).
	clread '[ 	]*' 'EOT' 'OOB' 'DEL' 'EOL'

	# Check if the class list and state table header match.
	line=(${REPLY%%[ 	]\/\/*})
	test "${clnames[*]}" != "${line[*]}" &&
		err "fatal: classes and state header don't match (line $lineno)"

	read || err "$eof"; let lineno++

	# Skip blanks directly beneath the header.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	# Read the state table until a blank or indented line.
	until test -z "${REPLY//[ 	]/}" -o -z "${REPLY##[ 	]*}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		stnames=(${stnames[@]} $line)

		state=()
		for entry in ${line[@]:1}
		do
			# Fail on non-positive entry numbers.
			test $entry -le 0 &&
				err "fatal: non-positive entry number (line $lineno)"

			# Make the numbers zero-based.
			let entry--

			state=(${state[@]} $entry)
		done

		test "${#state[@]}" -ne "$clcount" &&
			err "fatal: wrong entry count in state: $line (line $lineno)"
		states[${#states[@]}]="${state[@]}"

		read || err "$eof"; let lineno++

		# Skip line comments, but break on a blank line.
		while test "${REPLY//[ 	]/}" -a -z "${REPLY##\/\/*}"
		do read || err "$eof"; let lineno++
		done
	done

	# Skip any more blanks if necessary.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	# Check if the entry table header is as expected.
	line=(${REPLY%%[ 	]\/\/*})
	if test $tag = 'kerx' -a $tbfmt -eq 4
	then
		case "${line[*]}" in
			"GoTo Mark? Advance? MatchPoints") acttype=1; vlpack=2;;
			"GoTo Mark? Advance? MatchAnchors") acttype=2; vlpack=2;;
			"GoTo Mark? Advance? MatchCoords") acttype=4; vlpack=4;;
			*) err "fatal: malformed entry table header (line $lineno)";;
		esac
	else
		test "${line[*]}" != "GoTo Push? Advance? KernValues" &&
			err "fatal: malformed entry table header (line $lineno)"
	fi

	read || err "$eof"; let lineno++

	# Skip blanks beneath the header.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	# Read entries until a blank line, or an indent (the Font Tools way).
	until test -z "${REPLY//[ 	]/}" -o -z "${REPLY##[ 	]*}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		let entry=${#gotos[@]}+1
		test $line -eq $entry ||
			err "fatal: wrong number for entry listed as $entry (line $lineno)"

		stname=${line[@]:1:1}
		indexof $stname ${stnames[@]}
		test $index -eq -1 &&
			err "fatal: state not found: $stname (line $lineno)"
		gotos=(${gotos[@]} $index)
		gtnames=(${gtnames[@]} $stname)

		flpush=(${flpush[@]} ${line[@]:2:1})
		fladvance=(${fladvance[@]} ${line[@]:3:1})
		actnames=(${actnames[@]} ${line[@]:4:1})
		actlines=(${actlines[@]} $lineno)

		read || err "$eof"; let lineno++

		# Skip comments, but break on a blank.
		while test "${REPLY//[ 	]/}" -a -z "${REPLY##\/\/*}"
		do read || err "$eof"; let lineno++
		done
	done

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let lineno++
	done

	# Read values until the end of file or a next subtable header.
	until test -z "$REPLY" -o -z "${REPLY##Type[ 	]*}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		vlnames=(${vlnames[@]} $line)
		vlindices=(${vlindices[@]} ${#values[@]})

		read || break; let lineno++

		# Skip blanks beneath the kern list name.
		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || break 2; let lineno++
		done

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
				line=(${REPLY%%[ 	]\/\/*})
				test $line != "${field}" &&
					err "fatal: values for $field glyph expected (line $lineno)"
				test ${#line[@]} -ne $(( vlnreq + 1 )) &&
					err "fatal: wrong number of values (line $lineno)"

				value=${line[@]:1}

				if test $acttype -eq 1 -o $acttype -eq 2
				then
					# Fail on non-positive point/anchor index value.
					test $value -le 0 &&
						err "fatal: non-positive index value (line $lineno)"

					# Make the indices zero-based.
					let value--
				fi

				values=(${values[@]} $value)

				read || if test $field = 'Marked'
					then err "$eof"
					else break
					fi; let lineno++

				until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
				do read || if test $field = 'Marked'
					then err "$eof"
					else break 2
					fi; let lineno++
				done
			done

			# Don't bother with other formats at this point.
			continue
		fi

		# Read values in all the indented lines beneath the name.
		while test -z "${REPLY##[ 	]*}"
		do
			line=(${REPLY%%[ 	]\/\/*})

			# Fail on a reset value in a non-cross-stream table.
			test $flcross != 'yes' -a -z "${REPLY##*Reset*}" &&
				err "fatal: kern reset in a non-cross-stream table (line $lineno)"

			values=(${values[@]} ${line[@]})

			read || break 2; let lineno++

			# Skip blanks between the values, if any.
			until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
			do read || break 3; let lineno++
			done
		done
	done

	# Now with the values parsed match their indices to actions.
	for i in ${!actnames[@]}
	do
		name=${actnames[$i]}
		action=-1

		if test $name != 'none'
		then
			indexof $name ${vlnames[@]}
			test $index -eq -1 &&
				err "fatal: kern values not defined: $name (line ${actlines[$i]})"
			action=$index
		fi

		actions[$i]=$action
	done

	# Pre-compute the end-of-list marker count prior to each action.
	if test $tag = 'kerx' -a $tbfmt -eq 1
	then
		let prev=0 nmarks=0; for i in ${!vlindices[@]}
		do
			curr=${vlindices[$i]}

			# Increase the count for non-empty actions only.
			test $curr -gt $prev &&
				let nmarks++

			eolmarks[$i]=$nmarks
			prev=$curr
		done

		let eolmarkcount=$nmarks+1
	fi

	if test $tag = 'kerx'
	then
		# Filter out glyph segments for the class lookups.
		lucollapse $glstart $glend

		let luhead=6*2 # Size of a segmented lookup table header
		let lumapsize=2*2+$lusize # Mapping size
		let lumapcount=$lusegcount+1 # Number of mappings
	else
		let luhead=2*2 # Size of a trimmed lookup array header
		let lumapsize=$lusize # Mapping size
		let lumapcount=$glend-$glstart+1 # Number of mappings
	fi

	let lulen=$luhead+$lumapcount*$lumapsize # Class lookup table length
	let lupad=$lulen/$ver%2*$ver
	let stoff=$luoff+$lulen+$lupad # State table offset
	let stlen=${#states[@]}*$clcount*$trsize # State table length
	let stpad=$stlen/$ver%2*$ver
	let etoff=$stoff+$stlen+$stpad # Entry table offset
	let etlen=${#gotos[@]}*$etsize # Entry table length
	let etpad=$etlen/$ver%2*$ver
	let vloff=$etoff+$etlen+$etpad # Kern values offset
	let vllen=${#values[@]}*$vlsize # Values length
	let vllen+=$eolmarkcount*$vlsize # the end-of-list markers, if any
	let vlpad=$vllen/$ver%2*$ver
	let tblen=$tbhead+$vloff+$vllen+$vlpad # Subtable length

	printf "\n"
	printf "\t<!-- Subtable No. %d -->\n" $(( ++tbno ))
	printf "\n"

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

		printd "%04X" 2 "First glyph" $glstart
		printd "%04X" 2 "Glyph count" $lumapcount

		for i in $(seq $glstart $glend)
		do
			class=${luarr[$i]-1}
			printd "%02X" $lumapsize "${glnames[$i]}: ${clnames[$class]}" $class
		done
	fi

	pad $lupad

	# Print at least stubs of class names along the transitions.
	printf "\n\t                            <!-- "
	for name in ${clnames[@]}
	do printf "%-*.*s " $(( 2*ver )) $(( 2*ver )) $name
	done
	printf " -->\n"

	for i in ${!states[@]}
	do
		printd "%0*X " $(( trsize * clcount )) "${stnames[$i]}" $(

			for trans in ${states[$i]}
			do echo $(( trsize * 2 )) $trans
			done

			)
	done

	pad $stpad

	printf "\n"
	for i in ${!gotos[@]}
	do
		goto=${gotos[$i]}
		action=${actions[$i]}

		flags=0
		test ${flpush[$i]} = 'yes' && let flags+=16#8000
		test ${fladvance[$i]} = 'yes' || let flags+=16#4000

		comment="$(printf "%02X" $i) ${gtnames[$i]}"

		if test $tag = 'kerx'
		then
			if test $action -ge 0
			then
				let vlindex=${vlindices[$action]}
				if test $tbfmt -eq 4
				then
					let vlindex/=$vlpack
				else
					let vlindex+=${eolmarks[$action]}
				fi
			else
				let vlindex=16#FFFF
			fi

			printd "%04X %04X %04X" $etsize "$comment" $goto $flags $vlindex
		else
			# Use byte offsets for the old table.
			let goto=$stoff+$goto*$clcount

			test $action -ge 0 &&
				let flags+=$vloff+${vlindices[$action]}*$vlsize

			printd "%04X %04X" $etsize "$comment" $goto $flags
		fi
	done

	pad $etpad

	printf "\n"
	let v=0; for i in ${!vlindices[@]}
	do
		nextval=${vlindices[(( i+1 ))]=${#values[@]}}
		let count=$nextval-${vlindices[$i]}
		let len=$count*$vlsize
		test $tag = 'kerx' -a $tbfmt -eq 1 &&
			let len+=$vlsize # end-of-list value

		printd "%04X " $len "${vlnames[$i]}" $(

			let w=$v; while test $w -lt $nextval
			do
				value=${values[$w]}

				test $value = 'Reset' &&
					let value=16#8000 # cross-stream reset flag

				test $value -lt 0 &&
					let value=16#10000+$value # 2\'s complement

				if test $tag != 'kerx'
				then
					# Clear the least significant bit
					let value-=$value%2

					test $(( w + 1 )) -eq $nextval &&
						let value+=1 # end-of-list flag
				fi

				echo $value

				test $tag = 'kerx' -a $tbfmt -eq 1 -a $(( w + 1 )) -eq $nextval &&
					echo $(( 16#FFFF )) # end-of-list value

				let w++
			done

			)

		let v+=$count
	done

	pad $vlpad

done

printf "\n</genericSFNTTable>\n"

} <$kif >$kerx

# Don't bother fusing if that's a dry run.
test $dry && exit

# Check if ftxdumperfuser is available.
type ftxdumperfuser &>/dev/null ||
	err "fatal: couldn't find the ftxdumperfuser"

# Fuse the data files into the font.
for data in $ankr $kerx
do ftxdumperfuser -g -t $(basename -s .xml $data) -d $data $ttf
done

