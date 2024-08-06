## ---------------------------------- echo -------------------------------------
print("Importing matrix aux script.")

vector_to_symmetric_matrix = function(vector, vector_include_diagonal = FALSE, fill_diagonal = 0) {
    # Calculate the size of the matrix
    L = length(vector)
    if (vector_include_diagonal) {
        # Formula for the case where the diagonal is included
        n = (-1 + sqrt(1 + 4 * 2 * L)) / 2
    } else {
        # Formula for the case where the diagonal is excluded
        n = (1 + sqrt(1 + 8 * L)) / 2
    }

    # Check if n is nearly an integer
    if (abs(n - round(n)) > 1e-10) {
        stop("The length of the vector does not correspond to a triangular number.")
    }

    n = round(n)  # Now safely round n
    
    # Validate that when n is used to compute the triangular number, we get L
    if (vector_include_diagonal) {
        if (n * (n + 1) / 2 != L) {
            stop("The length of the vector does not correspond to a valid symmetric matrix including the diagonal.")
        }
    } else {
        if (n * (n - 1) / 2 != L) {
            stop("The length of the vector does not correspond to a valid upper triangle (excluding diagonal).")
        }
    }

    # Create an n x n matrix and initialize diagonal values if not included in vector
    matrix = matrix(fill_diagonal, n, n)

    # Assign the vector to the appropriate parts of the matrix
    if (vector_include_diagonal) {
        matrix[lower.tri(matrix, diag = TRUE)] = vector
    } else {
        matrix[upper.tri(matrix)] = vector
    }

    # Make the matrix symmetric
    ## diag(matrix) will give the diagonal of matrix in vector (matrix --> vector), diag(diag(matrix)) will use the diagonal vector to build a diagonal matrix which diagonal is the diagonal vector.
    matrix = matrix + t(matrix) - diag(diag(matrix)) 

    return(matrix)
}
