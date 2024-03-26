# -------------------------------  需要自己复制的东西 -----------------------------
#import sys
#import importlib
#sys.path.append('0aux_script')
#self_function = importlib.import_module("import_python")
#self_function = importlib.reload(self_function)
# ------------------------------- Base --------------------------------------



def dirname(path):
    import os
    return os.path.dirname(path)


def basename(path):
    import os
    return os.path.basename(path)
# -------------------------------  DataFrame  -------------------------------
def head(data,num):
    return(data.head(num))


def unique(data):
    import pandas as pd
    if isinstance(data, pd.DataFrame):
        series = data.values.flatten()
        data_type = 'DataFrame'
    elif isinstance(data, (list, tuple)):
        series = pd.Series(data).values
        data_type = type(data).__name__
    else:
        raise ValueError("Input data type not supported. Use DataFrame, list, or tuple.")
    
    unique_series = pd.Series(series).unique()
    
    print(f"Detect type: {data_type}, Unique Values: {len(unique_series)}, Total Count: {len(series)}")
    return unique_series


def column_to_rownames(data,colname):
    df_new=data.set_index(colname,
                        drop=True,
                        append=False,
                        inplace=False,
                        verify_integrity=True)
    return(df_new)


def select(data, *columns_to_select):
    df_selected = data[list(columns_to_select)]
    return df_selected






def write_df_wrap(df, file_name, save_dir=".", file_fmt="tsv", rowname="rowname", col_names=True, prompt=True):
    # Import packages
    import os
    import pandas as pd
    import pathlib

    # Convert to DataFrame if not already
    if not isinstance(df, pd.DataFrame):
        df = pd.DataFrame(df)
    
    # Create the save directory if it doesn't exist
    os.makedirs(save_dir, exist_ok=True)
    
    # Construct the file path
    file_name = f"{file_name}.{file_fmt}"
    file_name = pathlib.PurePath(file_name).name
    fig_path = os.path.join(save_dir, file_name)
    
    # Check if the index has been modified
    if not df.index.equals(pd.RangeIndex(start=0, stop=len(df))):
        # Create a new DataFrame with the modified index and insert it
        new_df = df.copy()

        ## if the index alread have name, then use it
        if not new_df.index.name is None:
            rowname = new_df.index.name

        new_df.insert(0, rowname, df.index)
        print(f"Modified index added as the first column '{rowname}' in the new DataFrame.")
    else:
        # If the original index hasn't been touched, use the original DataFrame
        new_df = df.copy()
        print(f"Original index without being touched, using the original DataFrame.")
    
    # Save in the specified format without index name
    if file_fmt == "tsv":
        new_df.to_csv(fig_path, sep='\t', index=False, header=col_names)
    elif file_fmt == "csv":
        new_df.to_csv(fig_path, index=False, header=col_names)
    elif file_fmt == "xlsx":
        new_df.to_excel(fig_path, index=False, header=col_names)
    else:
        raise ValueError("Only support tsv, csv, and xlsx file formats.")
    
    # Prompt if specified
    if prompt:
        print(f"Successfully saved DataFrame to {fig_path}")



