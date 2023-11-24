set -e











#help page
script_path=$(realpath $0)
if [ "$1" = "-h" ] || [ "$1" = "--help" ] || [ "$1" = "" ];then 
    echo "
    The path of the script is $script_path.
    Using samtools and minimap2/bwa to calculate the sequence depth. Use python to summary, so pandas is needed.
    "
    echo '
    $1 -- reference directory (Should only contains reference sequence, each reference files will only generate  1 depth)
    $2 -- mode (long, or short, short is still under development)
    $3 -- reads1 (The reads you want to map to the reference)
    $4 -- reads2 (If you are short mode)
    '
    exit 0
fi

#user function
echoo(){
    echo -e "\n$1\n"
}

#user arguments

ref_dir=$1
mode=$2
reads1=$3
reads2=$4

## define num of threads
threads=$(cat /proc/cpuinfo | grep "processor" |wc -l)

if [ "$mode" = "long" ];then
    echoo "Reference directory=$ref_dir, mode=$mode, reads=$reads1, threads=$threads"
elif [ "$mode" = "short" ];then
    echoo "Reference directory=$ref_dir, mode=$mode, read1=$reads1, read2=$reads2, threads=$threads"
else
    echoo "Reference directory=$ref_dir, mode=$mode, read1=$reads1, read2=$reads2, threads=$threads. Maybe something wrong, please check again."
    exit 1
fi





#check parameters

## check reference directory
if [ -z "$ref_dir" ];then
    echoo "Should input reference directory"
    exit 1
fi

## check mode
if [ -z "$mode" ];then
    echo "Should input mode"
    exit 1
    
else
    if [ "$mode" != "long" ] && [ "$mode" != "short" ];then
        echoo "Wrong mode, should be long or short only"
        exit
    fi
fi

## check reads
if [ -z "$reads1" ];then
    echo "Should input reads."
    exit
fi

## if mode=="short", check single or pair
if [ "$mode" = "short" ];then
    if [ -z "$reads2" ];then
        mode="short_single"
    else
        mode="short_pair"
    fi
fi

## prompt
echoo "Reference directory is $ref_dir; Mode is $mode."









#remove previous file

remove_func(){
    if [ "$mode" = "long" ];then
        find $ref_dir -type f|grep ".mmi\|.sam"|parallel "rm {}"
    fi
}
remove_func






#check program
p_short=$(which bwa)
p_short="$p_short mem"
p_long=$(which minimap2)
p_sam=$(which samtools)


if [ "$mode" = "long" ];then

    if [ -z $p_long ];then
        echoo "No minimap2."
        exit 1
    else
        echoo "The path of long mapper is $p_long"
    fi

elif [ "$mode" = "short" ];then

    if [ -z $p_short ];then
        echoo "No bwa."
        exit
    else
        echoo "The path of short mapper is $p_short"
    fi
fi


if [ -z "$p_sam" ];then
    echoo "No samtools."
    exit 1
else
    echoo "The path of samtools is $p_sam"
fi










#mapping
if [ "$mode" = "long" ];then
    
    ## index the reference
    find $ref_dir -type f | parallel "$p_long -d {}.mmi {} "
    
    ## mapping
    find $ref_dir -type f -name "*.mmi"| parallel -j 1 "$p_long -ax map-ont -t $threads {} $reads1 > {}.sam "

elif [ "$mode" = "short_single" ];then
    
    ## mapping
    find $ref_dir -type f|parallel -j 1 "$p_short -t $threads {} $reads1 > {}.sam "
    
elif [ "$mode" = "short_pair" ];then
    
    ## mapping
    find $ref_dir -type f|parallel -j 1 "$p_short -t $threads {} $reads1 $reads2 > {}.sam "
    
fi








# to bam and sort
find $ref_dir -type f -name "*.sam"|parallel -j 1 "samtools view -uS -@$threads {}|samtools sort -@$threads -o {}.sort.bam -"
    
# index bam
find $ref_dir -type f -name "*.sort.bam"|parallel -j 1 "samtools index -@$threads {}"

# calculate depth
find $ref_dir -type f -name "*.sort.bam"|parallel "samtools depth -aa {} > {}.depth"







# summary
echoo "\n\nSummary depth files"
py_script="$(dirname $script_path)/script_depth.py"
python $py_script $ref_dir


## prompt
echoo "All finished."
