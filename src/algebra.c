#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    if ((a.rows != b.rows) || (a.cols != b.cols))
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] + b.data[i][j];
            }
        }
        return c;
    }
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    if ((a.rows != b.rows) || (a.cols != b.cols))
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] - b.data[i][j];
            }
        }
        return c;
    }
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.cols != b.rows)
    {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, b.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < b.cols; j++)
            {
                c.data[i][j] = 0;
                for (int k = 0; k < a.cols; k++)
                {
                    c.data[i][j] += a.data[i][k] * b.data[k][j];
                }
            }
        }
        return c;
    }
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    Matrix c = create_matrix(a.rows, a.cols);
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            c.data[i][j] = k * a.data[i][j];
        }
    }
    return c;
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    Matrix c = create_matrix(a.cols, a.rows);
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            c.data[j][i] = a.data[i][j];
        }
    }
    return c;
}

Matrix by_matrix(Matrix a, int i, int j)
{
    Matrix c = create_matrix(a.rows - 1, a.cols - 1);
    int row, col;
    row = col = 0;
    for (int m = 0; m < a.rows - 1; m++)
    {
        row = (m < i ? m : m + 1);
        for (int n = 0; n < a.cols - 1; n++)
        {
            col = (n < j ? n : n + 1);
            c.data[m][n] = a.data[row][col];
        }
    }
    return c;
}

double det_matrix(Matrix a)
{
    // ToDo
    if (a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    else if (a.rows == 1)
    {
        return a.data[0][0];
    }
    else if (a.rows == 2)
    {
        return a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0];
    }
    else
    {
        double det = 0;
        for (int i = 0; i < a.rows; i++)
        {
            det += a.data[0][i] * det_matrix(by_matrix(a, 0, i)) * (i % 2 ? -1 : 1);
        }
        return det;
    }
}

Matrix inv_matrix(Matrix a)
{
    // ToDo
    if (a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return create_matrix(0, 0);
    }
    if (!det_matrix(a))
    {
        printf("Error: The matrix is singular.\n");
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = ((i + j) % 2 ? -1 : 1) * det_matrix(by_matrix(a, j, i));
            }
        }
        return scale_matrix(c, 1 / det_matrix(a));
    }
}

void swap_row(Matrix *a, int i, int j)
{
    double tmp;
    for (int k = 0; k < a->cols; k++)
    {
        tmp = a->data[i][k];
        a->data[i][k] = a->data[j][k];
        a->data[j][k] = tmp;
    }
}

int rank_matrix(Matrix a)
{
    int rank = (a.rows < a.cols ? a.rows : a.cols);
    for (int i = 0; i < rank; i++)
    {
        if (a.data[i][i] == 0)
        {
            for (int j = 1; j + i < a.rows; j++)
            {
                if (a.data[i + j][i] != 0)
                {
                    swap_row(&a, i, i + j);
                    break;
                }
            }
        }
        if (a.data[i][i] != 0)
        {
            for (int j = i + 1; j < a.rows; j++)
            {
                double mul=a.data[j][i]/a.data[i][i];
                for (int k = i; k < a.cols; k++)
                {
                    a.data[j][k] -= a.data[i][k] * mul;
                }
            }
        }
    }
    //print_matrix(a);
    for (rank = 0; rank < a.rows && rank < a.cols; rank++)
    {
        if (a.data[rank][rank] == 0)
        {
            break;
        }
    }
    return rank;
}

double trace_matrix(Matrix a)
{
    // ToDo
    if (a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    else
    {
        double sum = 0.0;
        for (int i = 0; i < a.rows; i++)
        {

            sum += a.data[i][i];
        }
        return sum;
    }
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}