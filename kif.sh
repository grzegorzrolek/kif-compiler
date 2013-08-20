#!/bin/bash

# Kerning Input File compiler for TrueType AAT
# Copyright 2013 Grzegorz Rolek


# Prints a message to stderr and exits.
err () {
	echo >&2 $0: $1
	exit ${2-1}
}

# Make sure both arguments are given.
test $# != 2 &&
	err "usage: $(basename $0) <post.xml> <kern.kif>" 2

# Parse the 'post' table dump for an array of glyphs names.
glnames=($(sed -n 's/<PostScriptName ..* NameString=\"\(..*\)\".*>/\1/p' <$1))

# Remove comments from the KIF file.
kif="$(sed -e '/^\/\/.*/d' -e 's/[ 	]*\/\/.*//' $2)"

# Fills $index with index of a token within the list following the token.
indexof () {
	index=0
	token=$1
	shift
	for item
	do
		test $token = $item && return
		let index++
	done
	index=-1
}


printf "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n"
printf "<genericSFNTTable tag=\"kern\">\n"

# Print the table header before reading actual subtables.
ntables=$(grep -c '^Type ' <<<"$kif")
off=0 # current offset into the table

printf "\t<dataline offset=\"%08X\" hex=\"%04X%04X\"/> <!-- %s -->\n" \
	$off 1 0 "Table version" && let off+=4

printf "\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
	$off $ntables "No. of subtables" && let off+=4

# Read the KIF subtable by subtable until the end of file.
until test $eof
do

	tabfmt=0 # Subtable's coverage flags and format
	classes=() # GID-indexed array of classes
	clnames=(EOT OOB DEL EOL) # Class names
	unset glstart # First glyph assigned to a class
	unset glend # Last glyph assigned to a class
	states=() # State records
	stnames=() # State names
	gotos=() # Next states
	gtnames=() # Names of the next states
	actions=() # Kern value lists to apply
	actnames=() # Names of kern value lists to apply
	values=() # Kerning values
	vlnames=() # Names of the kern value lists
	vlindices=() # Indexes of values beginning the lists

	until test "$REPLY"
	do read
	done
	line=($REPLY)
	test $line != "Type" &&
		err "fatal: kerning type expected"
	case ${line[@]:1} in
		Contextual) tabfmt=1;;
		*) err "fatal: unknown kerning type: ${line[@]:1}";;
	esac

	unset REPLY
	until test "$REPLY"
	do read
	done
	line=($REPLY)
	test $line != "Orientation" &&
		err "fatal: kerning orientation expected"
	case ${line[@]:1} in
		V) let tabfmt+=16#8000;;
		H) ;;
		*) err "fatal: bad orientation flag: ${line[@]:1}";;
	esac

	unset REPLY
	until test "$REPLY"
	do read
	done
	line=($REPLY)
	if test $line = "Cross-stream"
	then
		case ${line[@]:1} in
			yes) let tabfmt+=16#4000;;
			no) ;;
			*) err "fatal: bad cross-stream flag: ${line[@]:1}";;
		esac

		unset REPLY
		until test "$REPLY"
		do read
		done
		line=($REPLY)
	fi

	if test $line = "Variation"
	then
		case ${line[@]:1} in
			yes) let tabfmt+=16#2000;;
			no) ;;
			*) err "fatal: bad variation flag: ${line[@]:1}";;
		esac

		unset REPLY
		until test "$REPLY"
		do read
		done
	fi

	let nclasses=4 # four built-in classes
	while true
	do
		# Skip blanks, but break on a state table header (an indent).
		test "$REPLY" || { read; continue; }
		grep -q '^[a-zA-Z]' <<<"$REPLY" || break

		line=($REPLY)
		clnames[${#clnames[@]}]=$line

		for glyph in ${line[@]:1}
		do
			indexof $glyph ${glnames[@]}
			test $index -eq -1 &&
				err "fatal: glyph not found: $glyph"
			classes[$index]=$nclasses
			test $index -lt ${glstart=$index} && glstart=$index
			test $index -gt ${glend=$index} && glend=$index
		done

		let nclasses++
		read
	done

	# Set an Out-of-Bounds class on glyphs inbetween the specified ones.
	for i in $(seq $glstart $glend)
	do test "${classes[$i]}" || classes[$i]=1
	done

	let nmappings=$glend-$glstart+1
	let clpad=$nmappings%2
	let cloff=10 # constant
	let stoff=$cloff+4+$nmappings+$clpad # 4 for the glyph range

	# Check if the class list and state table header match.
	header=($REPLY)
	test "${clnames[*]}" != "${header[*]}" &&
		err "fatal: classes and state header don't match"

	# Skip blanks directly beneath the header.
	unset REPLY
	until test "$REPLY"
	do read
	done
	while true
	do
		# Break on a blank line or an entry table header.
		test "$REPLY" || break
		grep -q '^[a-zA-Z]' <<<"$REPLY" || break

		line=($REPLY)
		stnames=(${stnames[@]} $line)

		# Make the entry numbers zero-based.
		state=()
		for entry in ${line[@]:1}
		do
			let entry--
			state=(${state[@]} $entry)
		done

		test "${#state[@]}" -ne "$nclasses" &&
			err "fatal: wrong entry count in state: $line"
		states[${#states[@]}]="${state[@]}"

		read
	done

	nstates=${#states[@]}
	let stpad=$nstates*$nclasses%2
	let etoff=$stoff+$nstates*$nclasses+$stpad

	# Skip any more blanks if necessary.
	until test "$REPLY"
	do read
	done

	# Check if the entry table header is as expected.
	header=($REPLY)
	test "${header[*]}" != "GoTo Push? Advance? KernValues" &&
		err "fatal: malformed entry table header"

	# Skip blanks beneath the header.
	unset REPLY
	until test "$REPLY"
	do read
	done
	while true
	do
		# Break on a blank, and an indent (the Font Tools way).
		test "$REPLY" || break
		grep -q '^[a-zA-Z0-9_]' <<<"$REPLY" || break

		line=($REPLY)
		let entry=${#gotos[@]}+1
		test $line -eq $entry ||
			err "fatal: wrong number for entry listed as $entry"

		stname=${line[@]:1:1}
		indexof $stname ${stnames[@]}
		test $index -eq -1 &&
			err "fatal: state not found: $stname"
		let goto=$stoff+$index*$nclasses
		gotos=(${gotos[@]} $goto)
		gtnames=(${gtnames[@]} $stname)

		action=0
		test ${line[@]:2:1} = 'yes' && let action+=16#8000 # Push?
		test ${line[@]:3:1} = 'yes' || let action+=16#4000 # Advance?
		actions=(${actions[@]} $action)
		actnames=(${actnames[@]} ${line[@]:4:1})

		read
	done

	let vloff=$etoff+${#gotos[@]}*4

	while true
	do
		# Skip blanks, but break on next subtable, or an end of file.
		test $eof && break
		test "$REPLY" || { read || eof='yes'; continue; }
		grep -q 'Type' <<<"$REPLY" && break

		line=($REPLY)
		vlnames=(${vlnames[@]} $line)
		vlindices=(${vlindices[@]} ${#values[@]})

		for value in ${line[@]:1}
		do
			# Make a 2's complement for a negative value.
			test $value -lt 0 && let value=16#10000+$value

			# Unset the least significant bit for each value.
			let value-=$value%2

			values=(${values[@]} $value)
		done

		# Set the list-end flag for the last value.
		values[(( ${#values[@]}-1 ))]=$(( value+=1 ))

		read || eof='yes'
	done

	let tablen=$vloff+${#values[@]}*2+8

	# Now with the values parsed calculate the offsets.
	for i in ${!actions[@]}
	do
		vlname=${actnames[$i]}
		action=${actions[$i]}

		if test $vlname != 'none'
		then
			indexof $vlname ${vlnames[@]}
			test $index -eq -1 &&
				err "fatal: kern values not found: $vlname"
			let action+=$vloff+${vlindices[$index]}*2
		fi

		actions[$i]=$action
	done

	# Start printing the subtable with headers and the class lookups.
	printf "\n\t<!-- Subtable No. %d -->\n" $(( ++tabno ))

	printf "\n\t<dataline offset=\"%08X\" hex=\"%08X\"/> <!-- %s -->\n" \
		$off $tablen "Subtable length" && let off+=4

	printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
		$off $tabfmt "Coverage/format" \
		$(( off += 2 )) 0 "Variation tuple index" && let off+=2

	printf "\n"
	printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
		$off $nclasses "Class count" \
		$(( off += 2 )) $cloff "Class lookup offset" \
		$(( off += 2 )) $stoff "State table offset" \
		$(( off += 2 )) $etoff "Entry table offset" \
		$(( off += 2 )) $vloff "Values offset" && let off+=2

	printf "\n"
	printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/> <!-- %s -->\n" \
		$off $glstart "First glyph" \
		$(( off += 2 )) $nmappings "Glyph count" && let off+=2

	for i in $(seq $glstart $glend)
	do
		class=${classes[$i]}
		printf "\t<dataline offset=\"%08X\" hex=\"%02X\"/> <!-- %s -->\n" \
			$off $class "${clnames[$class]}: ${glnames[$i]}" && let off+=1
	done

	# Pad the class lookups with zeros for word-alignment if necessary.
	test $clpad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"00\"/>\n" \
			$off && let off+=1

	# Print at least stubs of class names along the transitions.
	printf "\n\t                            <!-- "
	printf "%-2.2s " ${clnames[@]}
	printf " -->\n"

	for i in ${!states[@]}
	do
		printf "\t<dataline offset=\"%08X\" hex=\"" $off
		printf "%02X " ${states[$i]} && let off+=$nclasses
		printf "\"/> <!-- %s -->\n" ${stnames[$i]}
	done

	# Pad the state table if necessary.
	test $stpad -ne 0 &&
		printf "\t<dataline offset=\"%08X\" hex=\"00\"/>\n" \
			$off && let off+=1

	printf "\n"
	for i in ${!gotos[@]}
	do
		printf "\t<dataline offset=\"%08X\" hex=\"%04X %04X\"/> <!-- %02X %s -->\n" \
			$off ${gotos[$i]} ${actions[$i]} $i ${gtnames[$i]} && let off+=4
	done

	i=0
	j=0
	printf "\n"
	for value in ${values[@]}
	do
		# Print the list's name on its first value only.
		unset vlname
		test ${vlindices[$j]} && test $(( i++ )) -eq ${vlindices[$j]} &&
			vlname=" <!-- ${vlnames[(( j++ ))]} -->"

		printf "\t<dataline offset=\"%08X\" hex=\"%04X\"/>%s\n" \
			$off $value "$vlname" && let off+=2
	done

done <<<"$kif"

printf "\n</genericSFNTTable>\n"
exit
