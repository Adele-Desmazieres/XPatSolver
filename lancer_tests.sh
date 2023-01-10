

INPUT=$1
OUTPUT=$2
TIMEOUT=$3

rm tmp.txt
touch tmp.txt

printf "#!/bin/bash\n\n" > tmp.txt


cat $INPUT \
	| sed 's/ /\n/g' \
	| sed "s/\(.*\)/\1 === \"; time timeout ${TIMEOUT} .\/run \1/g" \
	| sed 's/^/(printf "\\n === /g' \
	| sed "s/$/ -search out.sol) >> ${OUTPUT} 2>\&1/g" \
	>> tmp.txt \

chmod u+x tmp.txt

./tmp.txt


