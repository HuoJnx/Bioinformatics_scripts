# help page
script_path=$(realpath $0)
if [ "$1" == '-h' ] || [ "$1" == '-help' ] || [ "$#" == 0 ];then echo "
    The path of the script is $script_path.
    Based on metaquast, give a better usage of metaquast results with python. Needs pandas, matplotlib, seaborn, tableone, pingouin.
    Besides, it will activate a conda environment call quast contains the quast utilities for metaquast, and then activate a conda environment call stat contains the other pyhon utilities.

    -i: input directory, contains all the fa/fasta/fa.gz/fasta.gz files you want to evaluate by metaquast.
    -r: reference list, will just be redirected to the --reference-list parameter in metaquast.
    -s: golden standard genome. It is highly recommanded because sometimes metaquast can not get the right genome from NCBI and results in wrong evaluation.
    -d: depth file, receive a table contains the strains (1st columns) and the depth (2nd columns), NO HEADER!
    -m: metadata file, receive a table contains the file names of genome (1st columns) and their groupby information (2nd columns), NO HEADER!
    -o: output directory
    "
    exit 0
fi


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/junsheng/BIN/anaconda3/bin/conda' 'shell.bash' 'hook' 2>/dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/junsheng/BIN/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/junsheng/BIN/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/junsheng/BIN/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<




set -e




#define user function
echoo(){
    echo -e "\n$1\n"
}

python_func(){
    python_script_path="$(dirname $script_path)/script_python_after_quast.py"
    conda activate stat
    echoo "Parameters for python processing: Input: $out_quast; Depth file: $depth_file; Golden standard genome: $gold_genome; Metadata: $meta_data; Output: $out_quast."
    python $python_script_path -i $out_quast -d $depth_file -s $gold_genome -m $meta_data -o $out_python
    echoo "Thank you for using, all finished."
    exit
}







# get arguments



while getopts "i:r:s:d:m:o:" arg
do
    case $arg in
    i)
        fa_dir=$OPTARG
        echoo "The directory containing all the fasta is $OPTARG"
        
        ;;
    s)
        gold_genome=$OPTARG
        echoo "The goal-standard genome is $OPTARG"
        ;;
    r)
        ref_list=$OPTARG
        echoo "The reference list is $OPTARG"
        ;;
    m)
        meta_data=$OPTARG
        echoo "The metadata table is $OPTARG"
        ;;
    d)
        depth_file=$OPTARG
        echoo "The sequence depth file is $OPTARG "
        ;;
    o)
         out_quast=$OPTARG
        echoo "The output directory is $OPTARG"
        ;;

    esac
done













# check argument

## check ref_list
if [ -z "$ref_list" ];then
    echoo "WARNING: You havn't assigned a reference list, so metaquast will guess the reference genome by mapping to SILVA."

fi

## check gold_genome and pre-process
if [ -z "$gold_genome" ];then
    echoo "WARNING: You really don't want a gold-standard genome for baseline validation? Even after you assign the reference list, the metaquast-downloaded genomes maybe not that accurate."
    gold_genome="None"

else
    gold_genome=$(echo $gold_genome|awk -F"/" '{print $NF}')
    gold_genome=$(echo $gold_genome|awk -F"." '{print $1}')
fi

## check out_quast
if [ -z "$out_quast" ];then
    echoo "Haven't define output directory, will use default 'out_quast'."
    out_quast="out_quast"
fi

## 这句一定要写在这里，如果写在 pre-process 那里，有可能调用 python 之前，out_python 并不存在，就会出错
out_python="$out_quast/out_after_quast_python"

## check meta_data
if [ -z "$meta_data" ];then
    echoo "WARNING: no meta_data file, won't be processed by python."
    meta_data="None"
fi

## check depth_file
if [ -z "$depth_file" ];then
    echoo "WARNING: no depth file, Won't show depth information in heatmap."
    depth_file="None"
fi

## check whether $out_quast exists, if exists will go to python process directly.
if [ -d "$out_quast" ];then
    
    echoo "WARNING: Directory for quast output exists, won't do it again. So directly use existed metaquast results, and the given depth file, golden standard genome and metadata for python processing."
    python_func
    exit 0
    
fi





# pre-process argument



threads=$(cat /proc/cpuinfo | grep "processor" |wc -l)




## get f_list, gold_genome should be at first
f_list1=$(find $fa_dir -type f -name $gold_genome)

f_list2=$(find $fa_dir -type f \( -name "*.fa" -o -name "*.fasta" -o -name "*.fa.gz" -o -name "*.fasta.gz" \)|grep -v "$gold_genome")

### to sort the f_list2 by name, otherwise will be in a messy.
f_list2=$(echo "$f_list2"|sort)

f_list=$(echo -e "$f_list1\n$f_list2")





# do metaquast
conda activate quast

if [ -z "$ref_list" ];then
    metaquast.py $f_list -o $out_quast -t $threads
else
    metaquast.py $f_list --references-list $ref_list -o $out_quast -t $threads
fi







if [ ! -z "$meta_data" ];then
    python_func
    echoo "Thank you for using, all finished."
else
    echoo "No meta_data file, all finished."
fi
