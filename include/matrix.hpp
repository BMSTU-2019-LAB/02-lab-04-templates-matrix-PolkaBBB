// Copyright 2018 Your Name <your_email>

#ifndef INCLUDE_MATRIX_HPP_
#define INCLUDE_MATRIX_HPP_

#include <math.h>
#include <stdio.h>

#include <iostream>
#include <limits>
#include <type_traits>

template <class T>
class Matrix {
  static_assert(std::is_arithmetic<T>::value, "Non-arithmetic type");
  int n;
  int m;
  T** M;

 public:
  Matrix(int n, int m) {
    this->n = n;
    this->m = m;
    M = new T*[n];
    for (int i = 0; i < n; i++) {
      M[i] = new T[m];
    }
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        M[i][j] = 0;
      }
    }
  }
  T* operator[](int index) const { return M[index]; }
  Matrix() {
    n = 0;
    m = 0;
    M = nullptr;
  }
  ~Matrix() {
    if (M != nullptr) {
      for (int i = 0; i < n; i++) {
        delete[] M[i];
      }
      delete[] M;
    }
  }
  Matrix(const Matrix<T>& copy) {
    n = copy.n;
    m = copy.m;
    M = new T*[n];
    for (int i = 0; i < n; i++) {
      M[i] = new T[m];
    }
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        M[i][j] = copy.M[i][j];
      }
    }
  }
  Matrix<T>& operator=(const Matrix<T>& M1) {
    for (int i = 0; i < n; i++) {
      delete[] M[i];
    }
    delete[] M;
    this->n = n;
    this->m = m;
    M = new T*[n];
    for (int i = 0; i < n; i++) {
      M[i] = new T[m];
    }
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        M[i][j] = M1.M[i][j];
      }
    }
    return *this;
  }
  int get_rows() const { return n; }
  int get_columns() const { return m; }

  friend Matrix<T> operator+(const Matrix<T>& M1, const Matrix<T>& M2) {
    if (M1.get_rows() == M2.get_rows() &&
        M1.get_columns() == M2.get_columns()) {
      Matrix<T> M3(M1.get_rows(), M1.get_columns());
      for (int i = 0; i < M1.get_rows(); i++) {
        for (int j = 0; j < M1.get_columns(); j++) {
          M3.M[i][j] = M1.M[i][j] + M2.M[i][j];
        }
      }
      return M3;
    } else {
      return Matrix();
    }
  }

  friend Matrix<T> operator-(const Matrix<T>& M1, const Matrix<T>& M2) {
    if (M1.get_rows() == M2.get_rows() &&
        M1.get_columns() == M2.get_columns()) {
     Matrix<T> M3(M1.get_rows(), M1.get_columns());
     for (int i = 0; i < M1.get_rows(); i++) {
      for (int j = 0; j < M1.get_columns(); j++) {
        M3.M[i][j] = M1.M[i][j] - M2.M[i][j];
      }
     }
     return M3;
    } else {
      return Matrix();
    }
  }
  friend Matrix<T> operator*(const Matrix<T>& M1, const Matrix<T>& M2) {
    if (M1.get_rows() == M2.get_rows() &&
        M1.get_columns() == M2.get_columns()) {
      Matrix<T> M3(M1.get_rows(), M2.get_columns());
      for (int i = 0; i < M1.get_rows(); i++) {
        for (int j = 0; j < M1.get_columns(); j++) {
          T sum = 0;
          for (int f = 0; f < M1.get_columns(); f++) {
            sum += M1.M[i][f] * M2.M[f][j];
          }
          M3.M[i][j] = sum;
        }
      }
      return M3;
    } else {
      return Matrix();
    }
  }
  friend bool operator==(const Matrix<T>& M1, const Matrix<T>& M2) {
    if (M1.get_rows() == M2.get_rows()
        && M1.get_columns() == M2.get_columns()) {
      if (std::is_floating_point<T>::value) {
        for (int i = 0; i < M1.get_rows(); i++) {
          for (int j = 0; j < M1.get_columns(); j++) {
            if (fabs(M1[i][j] - M2[i][j]) >
                std::numeric_limits<double>::epsilon())
              return false;
          }
        }
        return true;
      }
      for (int i = 0; i < M1.get_rows(); i++) {
        for (int j = 0; j < M1.get_columns(); j++) {
          if (M1[i][j] != M2[i][j]) return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }
  friend bool operator!=(const Matrix<T>& M1, const Matrix<T>& M2) {
    if (M1 == M2) {
      return false;
    } else {
      return true;
    }
  }
  Matrix<T> delete_element(const Matrix<T>& M1, int I, int J);
  T determinant(const Matrix<T>& M1);
  Matrix<T> inverse();
};
template <class T>
Matrix<T> Matrix<T>::delete_element(const Matrix<T>& M1, int I, int J) {
  Matrix<T> M2(M1.get_rows() - 1, M1.get_columns() - 1);
  int n2 = 0;
  int m2 = 0;
  for (int i = 0; i < M1.get_rows(); i++) {
    if (i != I) {
      for (int j = 0; j < M1.get_columns(); j++) {
        if (j != J) {
          M2[n2][m2] = M1[i][j];
          m2++;
        }
      }
      n2++;
      m2 = 0;
    }
  }
  return M2;
}
template <class T>
T Matrix<T>::determinant(const Matrix<T>& M1) {
  T det = 0;
  if (M1.get_rows() > 2) {
    for (int j = 0; j < M1.get_columns(); j++) {
      if ((j % 2) == 1) {
        det += M1[0][j] * (-1) * determinant(delete_element(M1, 0, j));
      } else {
        det += M1[0][j] * determinant(delete_element(M1, 0, j));
      }
    }
  } else {
    if (M1.get_rows() == 2) {
      det = (M1[0][0] * M1[1][1]) - (M1[0][1] * M1[1][0]);
    } else {
      det = M1[0][0];
    }
  }
  return det;
}
template <class T>
Matrix<T> Matrix<T>::inverse() {
  T det = determinant(*this);
  Matrix<T> M_M(n, m);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      if ((j % 2) == 1) {
        M_M[i][j] = (-1) * determinant(delete_element(*this, i, j));
      } else {
        M_M[i][j] = determinant(delete_element(*this, i, j));
      }
    }
  }
  Matrix<T> M_T(M_M.get_rows(), M_M.get_columns());
  for (int i = 0; i < M_M.get_rows(); i++) {
    for (int j = 0; j < M_M.get_columns(); j++) {
      M_T[j][i] = M_M[i][j];
      M_T[i][j] = M_M[j][i];
    }
  }
  Matrix<T> M_I(M_M.get_rows(), M_M.get_columns());
  for (int i = 0; i < M_M.get_rows(); i++) {
    for (int j = 0; j < M_M.get_columns(); j++) {
      M_I[i][j] = (1. / det) * M_T[i][j];
    }
  }
  return M_I;
}

#endif // INCLUDE_MATRIX_HPP_
