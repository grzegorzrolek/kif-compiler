#!/bin/bash

# Kerning Input File compiler for TrueType AAT
# Copyright 2013 Grzegorz Rolek


usage=$(cat <<EOF
usage: $(basename $0) [-l] [-n] [-p post.xml] [-a anchors.kif] kerning.kif font.ttf
EOF
)


# err message: print message to stderr and exit
err () {
	echo >&2 $0: $1
	exit ${2-1}
}

# indexof token list: find $index of token within list
indexof () {
	local token=$1
	shift
	index=0
	for item
	do
		test $token = $item && return
		let index++
	done
	index=-1
}

# Parse and reset the arguments.
args=$(getopt np:a:l $*)
test $? -ne 0 &&
	echo >&2 "$usage" && exit 2
set -- $args

tag='kerx'; v=2 # Table version, 'kerx' by default
for i
do
	case "$i" in
		-n) dry='yes'; shift;;
		-p) post=$2; shift 2;;
		-a) ankfile=$2; shift 2;;
		-l) tag='kern'; v=1; shift;;
		--) shift; break;;
	esac
done

# Make sure at least the kerning input file is given.
test $# -lt 1 &&
	echo >&2 "$usage" && exit 2

kif=$1 # the input file
ttf=$2 # the font file, if any

# Make sure either the 'post' table dump or the font file are given.
test -z "$post" -a -z "$ttf" &&
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

# Reads-in the classes into $clnames[$nclasses] and $luarr[$glstart..$glend].
readcl () {
	local term="$1" # termination line pattern
	shift

	clnames=($@) # Class names, with the ones provided
	nclasses=$# # Number of classes
	unset glstart glend # First/last glyph with a class assigned
	luarr=() # Glyph-to-class index lookup array

	# Read classes until a line with the termination pattern found.
	until test -z "${REPLY##$term}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		if test $line != '+'
		then
			nclasses=${#clnames[@]}
			clnames=(${clnames[@]} $line)
		fi

		for glyph in ${line[@]:1}
		do
			index=$(grep -n "\"$glyph\"" $post | cut -d : -f 1)
			test $index ||
				err "fatal: glyph not found: $glyph (line $l)"
			let index-=$postoff
			luarr[$index]=$nclasses
			test $index -lt ${glstart=$index} && glstart=$index
			test $index -gt ${glend=$index} && glend=$index
		done

		read || err "$eof"; let l++

		# Skip blanks and comments inbetween.
		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || err "$eof"; let l++
		done
	done

	# Update the number of classes.
	let nclasses++
}

# Does the calculations for the bsearch header's $brange $bsel $bshift.
bcalc () {
	local usize=$1
	local nunits=$2
	bsel=0 # i.e., an exponent
	while test $(( nunits >> bsel )) -gt 1
	do let bsel++
	done
	brange=$(( usize * 2**bsel ))
	bshift=$(( usize * (nunits - 2**bsel) ))
}

# Finds segments within $luarr array and puts them in $lusegs[$lusegcount].
lucollapse () {
	local start=$1
	local end=$2
	local i=$start
	lusegs=() # lookup segments
	lusegcount=0
	while test $i -le $end
	do
		unset value
		local value=${luarr[$i]}

		# Skip holes in the array.
		if test -z "$value"
		then
			let i++
			continue
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

# Prints the lookup array from $lusegs[$lusegcount] array and $mapsize.
printlu () {
	local arrname=$1 # values to use instead

	# Use different values if provided, e.g., offsets instead of indices.
	test $arrname &&
		eval local arr=(\${$arrname[@]})

	# Calculate the bsearch header from unit size and count.
	bcalc $mapsize $lusegcount

	printf "\n"
	printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
		$off 2 "Lookup format" \
		$(( off += 2 )) $mapsize "Unit size" \
		$(( off += 2 )) $lusegcount "No. of units" \
		$(( off += 2 )) $brange "Search range" \
		$(( off += 2 )) $bsel "Entry selector" \
		$(( off += 2 )) $bshift "Range shift" && let off+=2

	for i in ${!lusegs[@]}
	do
		local s=(${lusegs[$i]})
		local val=${arr[$i]-${s[2]}}
		local names="${glnames[${s[0]}]} - ${glnames[${s[1]}]}: ${clnames[${s[2]}]}"
		printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X %04X\"/> <!-- %s -->\n" \
			$off ${s[@]:0:2} $val "$names" && let off+=$mapsize
	done
	printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X %04X\"/> <!-- %s -->\n" \
		$off $(( 16#FFFF )) $(( 16#FFFF )) 0 "Guardian value" && let off+=$mapsize
}


eof="fatal: premature end of file"

# Parse the anchor input file if given.
if test $ankfile
then

	{

	l=0 # No. of line being parsed

	# Read the first line.
	read; let l++

	# Skip blank lines and line comments in front of the input file.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || break; let l++
	done

	printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
	printf "<genericSFNTTable tag=\"ankr\">\n"

	off=0 # Current offset into the table

	printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
		$off 0 "Table version" \
		$(( off += 2 )) 0 "Flags" && let off+=2

	let luoff=12 # Lookup table offset, constant
	let luhead=6*2 # Size of a bsearch lookup table header
	let lusize=2 # Size of the anchor offset lookup entry
	let mapsize=2*2+$lusize # Mapping size

	anchors=() # Anchor coordinates
	ankindices=() # Indices of anchors that start each new class

	# Read classes until the the anchor listing shows up.
	readcl 'AnchorList*'

	# Read the first class reference.
	read || err "$eof"; let l++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	# Prevent the first reference from being accidentally indented.
	test -z "${REPLY##[ 	]*}" &&
		err "fatal: class reference expected (line $l)"

	clrefs=() # Class names as referenced (vs. as defined)

	# Read anchors until the end of file.
	ankindex=0
	until test -z "$REPLY"
	do
		line=(${REPLY%%[ 	]\/\/*})
		indexof $line ${clnames[@]}
		test $index -eq -1 &&
			err "fatal: class not found: $line (line $l)"
		indexof $line ${clrefs[@]}
		test $index -ne -1 &&
			err "fatal: class referenced twice: $line (line $l)"
		clrefs[${#clrefs[@]}]=$line
		ankindices=(${ankindices[@]} $ankindex)

		read || break; let l++

		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || break 2; let l++
		done

		# Read coordinates in the indented lines beneath.
		while test -z "${REPLY##[ 	]*}"
		do
			line=(${REPLY%%[ 	]\/\/*})
			test $(( ${#line[@]} % 2 )) -ne 0 &&
				err "fatal: wrong number of coordinates (line $l)"
			anchors=(${anchors[@]} ${line[@]})

			let ankindex++

			read || break 2; let l++

			until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
			do read || break 3; let l++
			done
		done
	done

	# Resolve classes as defined into classes as referenced.
	i=$glstart
	while test $i -le $glend
	do
		# Skip null values or they'll default to zero.
		if test -z "${luarr[$i]}"
		then
			let i++
			continue
		fi

		clname=${clnames[${luarr[$i]}]}
		indexof $clname ${clrefs[@]}

		# Remove unused classes from lookup array, if any.
		test $index -eq -1 &&
			index=""

		luarr[$i]=$index
		let i++
	done

	# Use only the referenced class names from now on.
	clnames=(${clrefs[@]})
	unset clrefs

	# Filter out segments from the lookup array.
	lucollapse $glstart $glend

	let nmappings=$lusegcount+1 # No. of mappings
	let lulen=$luhead+$nmappings*$mapsize # Lookup table length
	let lupad=$lulen/2%2*2 # Padding
	let ankoff=$luoff+$lulen+$lupad # Anchor table offset

	printf "\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
		$off $luoff "Anchor lookup offset" \
		$(( off += 4 )) $ankoff "Anchors offset" && let off+=4

	# Translate anchor indices to offsets.
	ankoffsets=()
	for i in ${!lusegs[@]}
	do
		s=(${lusegs[$i]})
		let ankoffset=${ankindices[${s[2]}]}*4+${s[2]}*4
		ankoffsets[$i]=$ankoffset
	done

	# Print the lookup array, but with offsets instead of indices.
	printlu 'ankoffsets'

	# Pad the anchor lookups with zeros for word-alignment if necessary.
	test $lupad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/>\n" \
			$off $(( lupad * 2 )) 0 && let off+=$lupad

	val=0 # Current value's index
	let nanchors=${#anchors[@]}/2 # No. of all the anchors
	printf "\n"
	for i in ${!ankindices[@]}
	do
		nextank=${ankindices[$(( i + 1 ))]=$nanchors}
		let nclanchors=$nextank-$val/2 # No. of anchors in a class
		printf "\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
			$off $nclanchors ${clnames[$i]} && let off+=4

		while test $(( val / 2 )) -lt $nextank
		do
			xval=${anchors[$val]}
			yval=${anchors[$(( val + 1 ))]}
			let val+=2

			# Make a 2's complement for a negative value.
			test $xval -lt 0 && let xval=16#10000+$xval
			test $yval -lt 0 && let yval=16#10000+$yval

			printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X\"/>\n" \
				$off $xval $yval && let off+=4
		done
	done

	printf "</genericSFNTTable>\n"

	} <$ankfile >$ankr

fi

{

l=0 # No. of line being parsed

# Read the first line.
read; let l++

# Skip blank lines and line comments in front of the input file.
until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
do read || break; let l++
done

printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
printf "<genericSFNTTable tag=\"%s\">\n" $tag

let tabhead=4+2*2*$v # Subtable header size
let luoff=5*2*$v # Lookup table offset (length of a state table header)
let lusize=1*$v # Size of the lookup value
let trsize=1*$v # Transition size
let etsize=2+2*$v # Full entry size in the entry table
let vlsize=2 # Value size

off=0 # Current offset into the table

# Scan the input file for a number of subtables.
ntables=$(grep -c '^Type[ 	]' $kif)

# Print the table header before reading subtables.
printf "\t<dataline offset=\"%08X\" hex=\"%04X%04X\"/> <!-- %s -->\n" \
	$off $v 0 "Table version" && let off+=4

printf "\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
	$off $ntables "No. of subtables" && let off+=4

# Read the file subtable by subtable to the end of file.
while test "$REPLY" -a -z "${REPLY##Type[ 	]*}"
do

	vertical='no'
	crossstream='no'
	unset tabfmt acttype # Subtable format and action type
	states=() # State records
	stnames=() # State names
	gotos=() # Next states
	gtnames=() # Names of the next states
	flpush=() # Flags for the push action
	fladvance=() # Flags for the advance action
	actions=() # Kern value lists to apply
	actnames=() # Names of kern value lists to apply
	values=() # Kerning values
	vlnames=() # Names of the kern value lists
	vlindices=() # Indexes of values beginning the lists
	vlpack=1 # No. of values per record; depends on table format

	# Parse the input line, comments excluded.
	line=(${REPLY%%[ 	]\/\/*})
	test $line != "Type" &&
		err "fatal: kerning type expected (line $l)"
	case ${line[@]:1} in
		Contextual) tabfmt=1;;
		Attachment) tabfmt=4;;
		*) err "fatal: unknown kerning type: ${line[@]:1} (line $l)";;
	esac

	test $tabfmt -eq 4 -a $tag != 'kerx' &&
		err "fatal: attachment allowed in 'kerx' table only (line $l)"

	read || err "$eof"; let l++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	line=(${REPLY%%[ 	]\/\/*})
	test $line != "Orientation" &&
		err "fatal: kerning orientation expected (line $l)"
	case ${line[@]:1} in
		V) vertical='yes';;
		H) ;;
		*) err "fatal: bad orientation flag: ${line[@]:1} (line $l)";;
	esac

	read || err "$eof"; let l++

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	line=(${REPLY%%[ 	]\/\/*})
	if test $line = "Cross-stream"
	then
		case ${line[@]:1} in
			yes) crossstream='yes';;
			no) ;;
			*) err "fatal: bad cross-stream flag: ${line[@]:1} (line $l)";;
		esac

		read || err "$eof"; let l++

		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || err "$eof"; let l++
		done
	fi

	# Read classes until a state table header (indented line).
	readcl '[ 	]*' 'EOT' 'OOB' 'DEL' 'EOL'

	# Check if the class list and state table header match.
	line=(${REPLY%%[ 	]\/\/*})
	test "${clnames[*]}" != "${line[*]}" &&
		err "fatal: classes and state header don't match (line $l)"

	read || err "$eof"; let l++

	# Skip blanks directly beneath the header.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
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
				err "fatal: non-positive entry number (line $l)"

			# Make the numbers zero-based.
			let entry--

			state=(${state[@]} $entry)
		done

		test "${#state[@]}" -ne "$nclasses" &&
			err "fatal: wrong entry count in state: $line (line $l)"
		states[${#states[@]}]="${state[@]}"

		read || err "$eof"; let l++

		# Skip line comments, but break on a blank line.
		while test "${REPLY//[ 	]/}" -a -z "${REPLY##\/\/*}"
		do read || err "$eof"; let l++
		done
	done

	# Skip any more blanks if necessary.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	# Check if the entry table header is as expected.
	line=(${REPLY%%[ 	]\/\/*})
	if test $tag = 'kerx' -a $tabfmt -eq 4
	then
		case "${line[*]}" in
			"GoTo Mark? Advance? MatchPoints") acttype=1; vlpack=2;;
			"GoTo Mark? Advance? MatchAnchors") acttype=2; vlpack=2;;
			"GoTo Mark? Advance? MatchCoords") acttype=4; vlpack=4;;
			*) err "fatal: malformed entry table header (line $l)";;
		esac
	else
		test "${line[*]}" != "GoTo Push? Advance? KernValues" &&
			err "fatal: malformed entry table header (line $l)"
	fi

	read || err "$eof"; let l++

	# Skip blanks beneath the header.
	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	lact=() # Line numbers of actions for later reporting

	# Read entries until a blank line, or an indent (the Font Tools way).
	until test -z "${REPLY//[ 	]/}" -o -z "${REPLY##[ 	]*}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		let entry=${#gotos[@]}+1
		test $line -eq $entry ||
			err "fatal: wrong number for entry listed as $entry (line $l)"

		stname=${line[@]:1:1}
		indexof $stname ${stnames[@]}
		test $index -eq -1 &&
			err "fatal: state not found: $stname (line $l)"
		gotos=(${gotos[@]} $index)
		gtnames=(${gtnames[@]} $stname)

		flpush=(${flpush[@]} ${line[@]:2:1})
		fladvance=(${fladvance[@]} ${line[@]:3:1})
		actnames=(${actnames[@]} ${line[@]:4:1})
		lact=(${lact[@]} $l)

		read || err "$eof"; let l++

		# Skip comments, but break on a blank.
		while test "${REPLY//[ 	]/}" -a -z "${REPLY##\/\/*}"
		do read || err "$eof"; let l++
		done
	done

	until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
	do read || err "$eof"; let l++
	done

	# Read values until the end of file or a next subtable header.
	until test -z "$REPLY" -o -z "${REPLY##Type[ 	]*}"
	do
		line=(${REPLY%%[ 	]\/\/*})
		vlnames=(${vlnames[@]} $line)
		vlindices=(${vlindices[@]} ${#values[@]})

		read || break; let l++

		# Skip blanks beneath the kern list name.
		until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
		do read || break 2; let l++
		done

		if test $tabfmt -eq 4
		then
			# Expect specific value count in each attachment type.
			case $acttype in
				1) nvlreq=1;; # one point per glyph
				2) nvlreq=1;; # one anchor per glyph
				4) nvlreq=2;; # two coordinates per glyph
			esac

			# Read values for both marked and current glyphs.
			for field in Marked Current
			do
				line=(${REPLY%%[ 	]\/\/*})
				test $line != "${field}" &&
					err "fatal: values for $field glyph expected (line $l)"
				test ${#line[@]} -ne $(( nvlreq + 1 )) &&
					err "fatal: wrong number of values (line $l)"

				value=${line[@]:1}

				if test $acttype -eq 1 -o $acttype -eq 2
				then
					# Fail on non-positive point/anchor index value.
					test $value -le 0 &&
						err "fatal: non-positive index value (line $l)"

					# Make the indices zero-based.
					let value--
				fi

				values=(${values[@]} $value)

				read || if test $field = 'Marked'
					then err "$eof"
					else break
					fi; let l++

				until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
				do read || if test $field = 'Marked'
					then err "$eof"
					else break 2
					fi; let l++
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
			! test $crossstream = 'yes' &&
				test -z "${REPLY##*Reset*}" &&
				err "fatal: kern reset in a non-cross-stream table (line $l)"

			values=(${values[@]} ${line[@]})

			read || break 2; let l++

			# Skip blanks between the values, if any.
			until test "${REPLY//[ 	]/}" -a "${REPLY##\/\/*}"
			do read || break 3; let l++
			done
		done
	done

	# Now with the values parsed match their indices to actions.
	for i in ${!actnames[@]}
	do
		vlname=${actnames[$i]}
		action=-1

		if test $vlname != 'none'
		then
			indexof $vlname ${vlnames[@]}
			test $index -eq -1 &&
				err "fatal: kern values not defined: $vlname (line ${lact[$i]})"
			action=$index
		fi

		actions[$i]=$action
	done

	# Pre-compute the end-of-list marker count prior to each action.
	if test $tag = 'kerx' -a $tabfmt -eq 1
	then
		eolmarks=() # Marker count, action-indexed
		previndex=0
		nmarks=0
		for i in ${!vlindices[@]}
		do
			currindex=${vlindices[$i]}

			# Increase the count for non-empty actions only.
			test $currindex -gt $previndex &&
				let nmarks++

			eolmarks[$i]=$nmarks
			previndex=$currindex
		done
	fi

	if test $tag = 'kerx'
	then
		# Filter out glyph segments for the class lookups.
		lucollapse $glstart $glend

		let luhead=6*2 # Size of a segmented lookup table header
		let mapsize=2*2+$lusize # Mapping size
		let nmappings=$lusegcount+1 # No. of mappings
	else
		let luhead=2*2 # Size of a trimmed lookup array header
		let mapsize=$lusize # Mapping size
		let nmappings=$glend-$glstart+1 # No. of mappings
	fi

	let lulen=$luhead+$nmappings*$mapsize # Class lookup table length
	let lupad=$lulen/$v%2*$v # Padding
	let stoff=$luoff+$lulen+$lupad # State table offset
	let stlen=${#states[@]}*$nclasses*$trsize # State table length
	let stpad=$stlen/$v%2*$v # Padding
	let etoff=$stoff+$stlen+$stpad # Entry table offset
	let etlen=${#gotos[@]}*$etsize # Entry table length
	let etpad=$etlen/$v%2*$v # Padding
	let vloff=$etoff+$etlen+$etpad # Kern values offset
	let vllen=${#values[@]}*$vlsize # Values length
	test $tag = 'kerx' -a $tabfmt -eq 1 &&
		let vllen+=\($nmarks+1\)*$vlsize # the end-of-list markers
	let vlpad=$vllen/$v%2*$v # Padding
	let tablen=$tabhead+$vloff+$vllen+$vlpad # Subtable length

	# Start printing the subtable with headers and the class lookups.
	printf "\n\t<!-- Subtable No. %d -->\n" $(( ++tabno ))

	printf "\n\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
		$off $tablen "Subtable length" && let off+=4

	flcover=0
	test $vertical = 'yes' && let flcover+=16#80
	test $crossstream = 'yes' && let flcover+=16#40

	# Make the coverage field 2 bytes longer for the extended table.
	test $tag = 'kerx' && (( flcover <<= 16 ))

	printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/> <!-- %s -->\n" \
		$off $(( 4*v - 2 )) $flcover "Coverage" \
		$(( off += 2*v - 1 )) 2 $tabfmt "Format" && let off+=1

	printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/> <!-- %s -->\n" \
		$off $(( 4*v )) 0 "Variation tuple index" && let off+=2*$v

	test $acttype && vloff=$(( vloff + (acttype << 29) ))

	printf "\n"
	printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/> <!-- %s -->\n" \
		$off $(( 4*v )) $nclasses "Class count" \
		$(( off += 2*v )) $(( 4*v )) $luoff "Class lookup offset" \
		$(( off += 2*v )) $(( 4*v )) $stoff "State table offset" \
		$(( off += 2*v )) $(( 4*v )) $etoff "Entry table offset" \
		$(( off += 2*v )) $(( 4*v )) $vloff "Values offset" && let off+=2*$v

	# Print either the modern or the legacy lookup array.
	if test $tag = 'kerx'
	then
		printlu
	else
		printf "\n"
		printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
			$off $glstart "First glyph" \
			$(( off += 2 )) $nmappings "Glyph count" && let off+=2

		for i in $(seq $glstart $glend)
		do
			class=${luarr[$i]-1}
			names="${glnames[$i]}: ${clnames[$class]}"
			printf "\t<dataline offset=\"%08X\" hex=\"%02X\"/> <!-- %s -->\n" \
				$off $class "$names" && let off+=$mapsize
		done
	fi

	# Pad the class lookups with zeros for word-alignment if necessary.
	test $lupad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/>\n" \
			$off $(( lupad * 2 )) 0 && let off+=$lupad

	# Print at least stubs of class names along the transitions.
	printf "\n\t                            <!-- "
	for clname in ${clnames[@]}
	do printf "%-*.*s " $(( 2*v )) $(( 2*v )) $clname
	done
	printf " -->\n"

	for i in ${!states[@]}
	do
		printf "\t<dataline offset=\"%08X\" hex=\"" $off
		for gtno in ${states[$i]}
		do printf "%0*X " $(( trsize * 2 )) $gtno && let off+=$trsize
		done
		printf "\"/> <!-- %s -->\n" ${stnames[$i]}
	done

	# Pad the state table if necessary.
	test $stpad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/>\n" \
			$off $(( stpad * 2 )) 0 && let off+=$stpad

	printf "\n"
	for i in ${!gotos[@]}
	do
		goto=${gotos[$i]}
		action=${actions[$i]}

		flags=0
		test ${flpush[$i]} = 'yes' && let flags+=16#8000
		test ${fladvance[$i]} = 'yes' || let flags+=16#4000

		if test $tag = 'kerx'
		then
			if test $action -ge 0
			then
				let vlindex=${vlindices[$action]}
				if test $tabfmt -eq 4
				then
					let vlindex/=$vlpack
				else
					let vlindex+=${eolmarks[$action]}
				fi
			else
				let vlindex=16#FFFF
			fi

			printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X %04X\"/> <!-- %02X %s -->\n" \
				$off $goto $flags $vlindex $i ${gtnames[$i]} && let off+=$etsize
		else
			# Use byte offsets for the old table.
			let goto=$stoff+$goto*$nclasses

			test $action -ge 0 &&
				let flags+=$vloff+${vlindices[$action]}*$vlsize

			printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X\"/> <!-- %02X %s -->\n" \
				$off $goto $flags $i ${gtnames[$i]} && let off+=$etsize
		fi
	done

	test $etpad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/>\n" \
			$off $(( etpad * 2 )) 0 && let off+=$etpad

	val=0
	printf "\n"
	for i in ${!vlindices[@]}
	do
		printf "\t<dataline offset=\"%08X\" hex=\"" $off

		nextval=${vlindices[(( i+1 ))]=${#values[@]}}
		while test $val -lt $nextval
		do
			value=${values[$val]}

			# Make the special reset value into a proper flag.
			test $value = 'Reset' && let value=16#8000

			# Make a 2's complement for a negative value.
			test $value -lt 0 && let value=16#10000+$value

			if test $tag != 'kerx'
			then
				# Unset the least significant bit of each value.
				let value-=$value%2

				# Set the list-end flag for the last value in a list.
				test $(( val + 1 )) -eq $nextval && let value+=1
			fi

			printf "%04X " $value && let off+=$vlsize

			# Place an end-of-list marker for the extended table.
			test $tag = 'kerx' -a $tabfmt -eq 1 &&
				test $(( val + 1 )) -eq $nextval &&
				printf "%04X" $(( 16#FFFF )) && let off+=$vlsize

			let val++
		done

		printf "\"/> <!-- %s -->\n" ${vlnames[$i]}
	done

	test $vlpad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"%0*X\"/>\n" \
			$off $(( vlpad * 2 )) 0 && let off+=$vlpad

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

exit
